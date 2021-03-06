package opencl.executor

import java.nio.{ByteBuffer, ByteOrder}

import ir._
import opencl.ir.{Bool, Double, Float, Int}
import org.junit.Assert.assertArrayEquals
import org.junit.{AfterClass, BeforeClass, Test}

import scala.reflect.ClassTag

class TestEncoder {
  import TestEncoder._

  @Test
  def encodeTuple(): Unit = {
    val buf = mkBuffer(12)
    buf.put(0, 1.toByte)
    buf.putInt(4, 42)
    buf.putFloat(8, 13.13f)

    val encoder = new Encoder(TupleType(Bool, TupleType(Int, Float)), 12)
    assertBufferEquals(buf, encoder.encode((true, (42, 13.13f))))
  }

  @Test
  def encodeFull1D(): Unit = {
    val size = 16
    val tyCon = (st: ScalarType) => ArrayType(st, size)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, size * _)

    assertBufferEquals(toBuffer(fArray), fEncoder.encode(fArray))
    assertBufferEquals(toBuffer(iArray), iEncoder.encode(iArray))
    assertBufferEquals(toBuffer(dArray), dEncoder.encode(dArray))
    assertBufferEquals(toBuffer(bArray), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged1D(): Unit = {
    val capacity = 32
    val size = 17
    val allocatedSize = (baseSize: Int) => Math.max(4, baseSize) + capacity * baseSize
    val tyCon = (st: ScalarType) => ArrayTypeWC(st, capacity)

    val (bArray, iArray, fArray, dArray) = get1DData(size)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocatedSize)

    def gold[T](array: Array[T], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocatedSize(baseSize))
      // Header: just the size
      buffer.putInt(size)
      buffer.position(Math.max(baseSize, 4))
      // Data: raw array
      val data = toBuffer(array)
      data.position(0)
      buffer.put(data)
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }

  @Test
  def encodeFull2D(): Unit = {
    val (sizeX, sizeY) = (4, 8)
    val allocSize = (baseSize: Int) => sizeX * sizeY * baseSize
    val tyCon = (st: ScalarType) => ArrayType(ArrayType(st, sizeY), sizeX)

    val (bArray, iArray, fArray, dArray) = get2DData(sizeX, sizeY)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    assertBufferEquals(toBuffer(fArray.flatten), fEncoder.encode(fArray))
    assertBufferEquals(toBuffer(iArray.flatten), iEncoder.encode(iArray))
    assertBufferEquals(toBuffer(dArray.flatten), dEncoder.encode(dArray))
    assertBufferEquals(toBuffer(bArray.flatten), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged2DPadding(): Unit = {
    val capX = 16
    val capY = 8
    val sizeX = 13
    val allocSize = (baseSize: Int) => Math.max(baseSize, 4) + capX * (Math.max(4, baseSize) + baseSize * capY)
    val tyCon = (st: ScalarType) => ArrayTypeWC(ArrayTypeWC(st, capY), capX)

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, (1, capY))
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val buffer = mkBuffer(allocSize(baseSize))

      // Header: just the size
      buffer.putInt(sizeX)
      buffer.position(Math.max(baseSize, 4))

      // Body: a flattened version of the 2D array padded with zeros
      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          buffer.putInt(arr.length)
          buffer.position(start + Math.max(baseSize, 4))
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + Math.max(baseSize, 4) + baseSize * capY)
      }

      buffer
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }

  @Test
  def encodeRagged2DOffsets(): Unit = {
    val capX = 8
    val sizeX = 5
    val yBounds = (1, 32)
    val tyCon = (st: ScalarType) => ArrayTypeWC(ArrayType(st), capX)
    // This allocated size is chosen based on our knowledge of the data, this
    // information cannot be retrieved from the type.
    // See the ScalaDoc comment at the top of `OpenCLMemoryAllocator.scala`
    val allocSize = (baseSize: Int) => {
      val alignment = Math.max(baseSize, 4)
       alignment * (1 + capX) + capX * (2 * alignment + baseSize * yBounds._2)
    }

    val (bArray, iArray, fArray, dArray) = get2DRaggedData(sizeX, yBounds)
    val (bEncoder, iEncoder, fEncoder, dEncoder) = getEncoders(tyCon, allocSize)

    def gold[T: ClassTag](array2D: Array[Array[T]], baseSize: Int): ByteBuffer = {
      val alignment = Math.max(baseSize, 4)
      val buffer = mkBuffer(allocSize(baseSize))

      // Header: just the size
      buffer.asIntBuffer().put(sizeX)
      // Offsets: `capX` integer values storing the offset in bytes between the
      //          beginning of the outer array and the beginning of each inner
      //          array.
      var ofs = alignment * (1 + capX)
      array2D.map(_.length).zipWithIndex.foreach {
        case (len, idx) =>
          buffer.position((1 + idx) * alignment)
          buffer.putInt(ofs)
          ofs += alignOn(len * baseSize, alignment) + 2 * alignment
      }
      buffer.position((1 + capX) * alignment)

      // A flattened version of the 2D array with *NO* padding.
      array2D.foreach {
        (arr: Array[T]) =>
          val start = buffer.position()
          // Header
          buffer.putInt(arr.length)
          buffer.position(start + alignment)
          buffer.putInt(arr.length)
          buffer.position(start + alignment * 2)
          // Content
          val encodedRow = toBuffer(arr)
          encodedRow.position(0)
          buffer.put(encodedRow)
          buffer.position(start + 2 * alignment + alignOn(arr.length * baseSize, alignment))
      }

      buffer
    }

    assertBufferEquals(gold(fArray, 4), fEncoder.encode(fArray))
    assertBufferEquals(gold(iArray, 4), iEncoder.encode(iArray))
    assertBufferEquals(gold(dArray, 8), dEncoder.encode(dArray))
    assertBufferEquals(gold(bArray, 1), bEncoder.encode(bArray))
  }

  @Test
  def encode4D(): Unit = {
    val ty = ArrayType(ArrayType(ArrayType(ArrayType(Int), 2), 2), 3)
    val array =
      Array(
        Array(
          Array(
            Array(1, 2, 3), Array(4, 5)
          ),
          Array(
            Array(6, 7), Array(8, 9, 10, 11)
          )
        ),
        Array(
          Array(
            Array(12), Array(13, 14, 15)
          ),
          Array(
            Array(16, 17, 18, 19), Array(20, 21)
          )
        ),
        Array(
          Array(
            Array(22), Array(23, 24)
          ),
          Array(
            Array(25, 26), Array(27, 28, 29, 30, 31)
          )
        )
      )
    val gold = Array(
      3 * 4, 28 * 4, 52* 4,
        // Element 0
        2 * 4, 13 * 4,
          // Element 0,0
          2 * 4, 7 * 4,
          3, 3, 1, 2, 3,   2, 2, 4, 5,
          // Element 0,1
          2 * 4, 6 * 4,
          2, 2, 6, 7,   4, 4, 8, 9, 10, 11,
        // Element 1
        2 * 4, 12 * 4,
          // Element 1,0
          2 * 4, 5 * 4,
          1, 1, 12,   3, 3, 13, 14, 15,
          // Element 1,1
          2 * 4, 8 * 4,
          4, 4, 16, 17, 18, 19,   2, 2, 20, 21,
        // Element 2
        2 * 4, 11 * 4,
          // Element 2,0
          2 * 4, 5 * 4,
          1, 1, 22,   2, 2, 23, 24,
          // Element 2,1
          2 * 4, 6 * 4,
          2, 2, 25, 26,   5, 5, 27, 28, 29, 30, 31
    )

    val encoder = new Encoder(ty, gold.length * 4)
    assertBufferEquals(toBuffer(gold), encoder.encode(array))
  }
}

object TestEncoder {
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }

  lazy val endianness: ByteOrder = if (Executor.isLittleEndian) ByteOrder.LITTLE_ENDIAN else ByteOrder.BIG_ENDIAN

  /** Instantiate 4 encoders for the 4 supported scalar types */
  def getEncoders(tyCon: ScalarType => ArrayType, allocSize: Int => Int): (Encoder, Encoder, Encoder, Encoder) = (
    new Encoder(tyCon(Bool), allocSize(1)),
    new Encoder(tyCon(Int), allocSize(4)),
    new Encoder(tyCon(Float), allocSize(4)),
    new Encoder(tyCon(Double), allocSize(8))
  )

  // ---
  // Random data generators
  // ---

  def get1DData(size: Int): (Array[Boolean], Array[Int], Array[Float], Array[Double]) = (
    Array.fill(size)(util.Random.nextBoolean()),
    Array.fill(size)(util.Random.nextInt()),
    Array.fill(size)(util.Random.nextFloat()),
    Array.fill(size)(util.Random.nextDouble())
  )

  def get2DData(sizeX: Int, sizeY: Int): (Array[Array[Boolean]], Array[Array[Int]],
                                           Array[Array[Float]], Array[Array[Double]]) = (
    Array.fill(sizeX, sizeY)(util.Random.nextBoolean()),
    Array.fill(sizeX, sizeY)(util.Random.nextInt()),
    Array.fill(sizeX, sizeY)(util.Random.nextFloat()),
    Array.fill(sizeX, sizeY)(util.Random.nextDouble())
  )

  def get2DRaggedData(sizeX: Int, yBounds: (Int, Int)): (Array[Array[Boolean]], Array[Array[Int]],
                                                         Array[Array[Float]], Array[Array[Double]]) = {
    val (minY, maxY) = yBounds
    assert(minY <= maxY)  // Sanity check
    val sizesY = Array.fill(sizeX)(minY + util.Random.nextInt(maxY - minY))
    (
      sizesY.map(Array.fill(_)(util.Random.nextBoolean())),
      sizesY.map(Array.fill(_)(util.Random.nextInt())),
      sizesY.map(Array.fill(_)(util.Random.nextFloat())),
      sizesY.map(Array.fill(_)(util.Random.nextDouble()))
    )
  }

  // ---
  // Some helper functions for working with buffers
  // ---

  def alignOn(n: Int, alignment: Int): Int = ((n + alignment - 1) / alignment ) * alignment

  def sizeOf(x: Any): Int = x match {
    case _: Boolean => 1
    case _: Int => 4
    case _: Float => 4
    case _: Double => 8
  }

  def toBuffer(vector: Array[_]): ByteBuffer = {
    val buffer = mkBuffer(vector.length * sizeOf(vector.head))
    vector.head match {
      case _: Boolean =>
        val array = vector.asInstanceOf[Array[Boolean]]
        buffer.put(array.map(b => (if (b) 1 else 0).toByte))
      case _: Int =>
        val array = vector.asInstanceOf[Array[Int]]
        buffer.asIntBuffer().put(array)
      case _: Float =>
        val array = vector.asInstanceOf[Array[Float]]
        buffer.asFloatBuffer().put(array)
      case _: Double =>
        val array = vector.asInstanceOf[Array[Double]]
        buffer.asDoubleBuffer().put(array)
    }
    buffer
  }

  /** Shorthand */
  def mkBuffer(size: Int): ByteBuffer = {
    val buffer = ByteBuffer.allocate(size)
    buffer.order(endianness)
    buffer
  }

  def assertBufferEquals(leftBuf: ByteBuffer, rightBuf: ByteBuffer): Unit = {
    assertArrayEquals(leftBuf.array, rightBuf.array)
  }
}
