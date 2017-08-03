package opencl.generator

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}

object TestCheckedArrayAccess {
  @BeforeClass def TestUnsafeArrayAccess() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}



class TestCheckedArrayAccess {
  @Test def TEST_ACCESS() : Unit = {
    val inputSize = Math.pow(2, 7).toInt
    val index = util.Random.nextInt(inputSize)
    val inputArr = Array.tabulate(inputSize)(_.toFloat)
    val gold = inputArr(index)
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Int, 1),
      (arr, ix) => {
        MapSeq(
          fun((index) => 
            toGlobal(id) o CheckedArrayAccess(index, -142.0f) $ arr
          )
        ) $ ix
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Float], runtime) = Execute(1,1)(accessKernel, inputArr, Array(index))
    println("Time: "+runtime)
    println("Gold: "+ gold)
    println("Output: "+ output(0))
    assert(output(0) == gold)
  }

  @Test def TEST_ACCESS_SORT_OF_2D() : Unit = {
    val inputSize = Math.pow(2, 4).toInt
    val inputArr = Array.tabulate(inputSize)((i) => Array.tabulate(inputSize)(_.toFloat))
    val index = util.Random.nextInt(inputSize)
    val gold = inputArr.map((row) => row(index))
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Int, 1),
      (arr, ix) => {
        MapSeq(
          fun((row) => 
            MapSeq(
              fun((index) =>
                toGlobal(id) o CheckedArrayAccess(index, -142.0f) $ row
              )
            ) $ ix
          )
        ) $ arr
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Float], runtime) = Execute(1,1)(accessKernel, inputArr, Array(index))
    println("Time: "+runtime)
    println("Gold: "+ gold.deep.mkString(", "))
    println("Output: "+ output.deep.mkString(", "))
    assertArrayEquals(output,gold, 0.0f)
  }

  @Test def TEST_ACCESS_2D() : Unit = {
    val inputSize = Math.pow(2, 7).toInt
    val inputArr = Array.tabulate(inputSize)((i) => Array.tabulate(inputSize)(_.toFloat))
    val indexArr = Array.tabulate(inputSize)((i) => i)
    val gold = inputArr.zip(indexArr).map{ case (row, index) => row(index) }
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), N),
      ArrayTypeWSWC(Int, N),
      (arr, ix) => {
        MapSeq(fun((indexRowPair) =>
          toGlobal(id) o CheckedArrayAccess(indexRowPair._0, -142.0f) $ indexRowPair._1
        )) $ Zip(ix, arr)
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Float], runtime) = Execute(1,1)(accessKernel, inputArr, indexArr)
    println("Time: "+runtime)
    println("Gold: "+ gold(0))
    println("Output: "+ output(0))
    assertArrayEquals(output,gold, 0.0f)
  }

  @Test def TEST_TUPLE_ACCESS() : Unit = {
    val inputSize = Math.pow(2, 7).toInt
    val index = util.Random.nextInt(inputSize)
    val inputArr = Array.tabulate(inputSize)((i) => (i,i))
    val gold = inputArr(index)
    val passArr = inputArr.map{case (i,j) => Array(i, j)}.flatten
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(TupleType(Int, Int), N),
      ArrayTypeWSWC(Int, 1),
      (arr, ix) => {
        MapSeq(
          fun((index) => 
            // MapSeq(idII) o Head() $ arr
//            MapSeq(t_id) o CheckedArrayAccess(index, Tuple(2)(-142, -142)) $ arr
            toGlobal(t_id) o CheckedArrayAccess(index, Tuple(2)(-142, -142)) $ arr
          )
        ) $ ix
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Int], runtime) = Execute(1,1)(accessKernel, passArr, Array(index))
    println("Time: "+runtime)
    println("Gold: "+ gold)
    println("Output: ("+ output(0)+","+output(1)+")")
    assert(output(0) == gold._1 && output(1) == gold._2)
  }

  @Test def TEST_OUT_OF_BOUNDS_ACCESS() : Unit = {
    val inputSize = Math.pow(2, 7).toInt
    val index = inputSize * 2
    val inputArr = Array.tabulate(inputSize)(_.toFloat)
    val gold = -142.0f
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Int, 1),
      (arr, ix) => {
        MapSeq(
          fun((index) =>
            toGlobal(id) o CheckedArrayAccess(index, -142.0f) $ arr
          )
        ) $ ix
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Float], runtime) = Execute(1,1)(accessKernel, inputArr, Array(index))
    println("Time: "+runtime)
    println("Gold: "+ gold)
    println("Output: "+ output(0))
    assert(output(0) == gold)
  }

  @Test def TEST_NEGATIVE_ACCESS() : Unit = {
    val inputSize = Math.pow(2, 7).toInt
    val index = -1
    val inputArr = Array.tabulate(inputSize)(_.toFloat)
    val gold = -142.0f
    val N = SizeVar("N")
    val accessKernel = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Int, 1),
      (arr, ix) => {
        MapSeq(
          fun((index) =>
            toGlobal(id) o CheckedArrayAccess(index, -142.0f) $ arr
          )
        ) $ ix
      }
    )
    println("Kernel:")
    println(Compile(accessKernel))
    println("===---===---===")
    val (output:Array[Float], runtime) = Execute(1,1)(accessKernel, inputArr, Array(index))
    println("Time: "+runtime)
    println("Gold: "+ gold)
    println("Output: "+ output(0))
    assert(output(0) == gold)
  }
}