package rewriting

import ir._
import ir.ast._
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, TestWithExecutor}
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.Test

object TestDerivingMatrixReuse extends TestWithExecutor

class TestDerivingMatrixReuse {

  val mSize = 16
  val kSize = mSize
  val nSize = mSize

  val matrixA = Array.tabulate(mSize, kSize)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
  val matrixB = Array.tabulate(kSize, nSize)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
  val transposedMatrixB = matrixB.transpose

  val gold = opencl.executor.Utils.matrixMatrixMultiply(matrixA, matrixB).flatten

  val N = SizeVar("N")
  val M = SizeVar("M")
  val K = SizeVar("K")

  val chunkSize = 4

  @Test
  def deriveReuseA(): Unit = {

    // Starting expression
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map fission
    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            MapSeq(
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
              MapSeq(fun(bCol => MapSeq(mult) $ Zip(aRow, bCol))) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Map-Reduce interchange
    val f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                  MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize))) o Transpose()
              o MapSeq(fun(bCol => MapSeq(mult) $ Zip(aRow, bCol)
            )) $ bCols
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize))) o Transpose()
              o Transpose() o MapSeq(fun(elemRowPair =>
              MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
              )) $ Get(elemRowPair, 1)
            )) $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Transpose o Transpose => id
    val f5 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) $ Zip(acc, c)
              ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize)))
              o MapSeq(fun(elemRowPair =>
              MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
              )) $ Get(elemRowPair, 1)
            )) $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f6 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Join() o MapSeq(fun( bCols =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) =>
                MapSeq(add) o fun(elemRowPair =>
                  Zip(toPrivate(MapSeq(fun(a => mult.apply(a, Get(elemRowPair, 0))))) $ Get(elemRowPair, 1), acc)
                ) $ c
              ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize)))
              $ Zip(aRow, Transpose() $ bCols)
          )) o Split(chunkSize) $ B
        )) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)
  }

  @Test
  def deriveReuseB(): Unit = {

    // Starting expression
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // split-join
    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          MapSeq(fun( aRow =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ B
          )) $ aRows
        )) o Split(chunkSize) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map-Map interchange
    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o MapSeq(fun( bCol =>
            MapSeq(fun( aRow =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Map fission
    val f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o MapSeq(fun( bCol =>
            Join() o MapSeq(fun( c => toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) $ c)) o
              MapSeq(fun(aRow => MapSeq(mult) $ Zip(aRow, bCol))) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Reduce interchange
    val f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize))) o Transpose() o
              MapSeq(fun(aRow => MapSeq(mult) $ Zip(aRow, bCol))) $ aRows
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f5 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize))) o Transpose() o
              Transpose() o MapSeq(fun(rowElemPair => MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1)))) $ Get(rowElemPair, 0))) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // Transpose o Transpose => id
    val f6 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) $ Zip(acc, c)),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize)))
              o MapSeq(fun(rowElemPair =>
              MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1)))) $ Get(rowElemPair, 0)
            )) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)

    // ReduceSeq o MapSeq fusion
    val f7 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Join() o MapGlb(fun( aRows =>
          TransposeW() o Join() o MapSeq(fun( bCol =>
            toGlobal(MapSeq(MapSeq(id))) o
              ReduceSeq(fun((acc, c) => MapSeq(add) o fun(rowElemPair =>
                Zip(acc, toPrivate(MapSeq(fun( aElem => mult.apply(aElem, Get(rowElemPair, 1))))) $ Get(rowElemPair, 0))) $ c),
                MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSize))
              ) $ Zip(Transpose() $ aRows, bCol)
          )) $ B
        )) o Split(chunkSize) $ A
      })

    val (output7: Array[Float], _) = Execute(1, mSize)(f7, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output7, 0.0f)
  }

  @Test
  def deriveReuseBothBInnermost(): Unit = {
    val chunkSizeN = 4
    val chunkSizeM = 4

    // Starting expression
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) $ B
        )) $ A
      })

    val (output: Array[Float], _) = Execute(1, mSize)(f, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output, 0.0f)

    // Reorder both sides
    val f1 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        MapGlb(fun( aRow =>
          Scatter(reorderStride(chunkSizeM)) o MapSeq(fun( bCol =>
            toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
          )) o ReorderStride(chunkSizeM) $ B
        )) $ A
      })

    val (output1: Array[Float], _) = Execute(1, mSize)(f1, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output1, 0.0f)

    // Map fission, pull reorder out
    val f2 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o
          MapGlb(fun( aRow =>
            MapSeq(fun( bCol =>
              toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
            )) o ReorderStride(chunkSizeM) $ B
          )) $ A
      })

    val (output2: Array[Float], _) = Execute(1, mSize)(f2, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output2, 0.0f)

    // Split-join
    val f3 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            MapSeq(fun( aRow =>
              MapSeq(fun( bCol =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) o ReorderStride(chunkSizeM) $ B
            )) $ aRows
          )) o Split(chunkSizeN) $ A
      })

    val (output3: Array[Float], _) = Execute(1, mSize)(f3, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output3, 0.0f)

    // Map-Map interchange
    val f4 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o MapSeq(fun( bCol =>
              MapSeq(fun( aRow =>
                toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
              )) $ aRows
            )) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output4: Array[Float], _) = Execute(1, mSize)(f4, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output4, 0.0f)

    // Split-join
    val f5 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o Join() o
              MapSeq(fun( bCols =>
                MapSeq(fun( bCol =>
                  MapSeq(fun( aRow =>
                    toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ aRows
                )) $ bCols
              )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output5: Array[Float], _) = Execute(1, mSize)(f5, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output5, 0.0f)

    // Map-Map interchange
    val f6 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          MapGlb(fun( aRows =>
            TransposeW() o Join() o
              MapSeq(fun( bCols =>
                TransposeW() o MapSeq(fun( aRow =>
                  MapSeq(fun( bCol =>
                    toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
                )) $ aRows
              )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output6: Array[Float], _) = Execute(1, mSize)(f6, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output6, 0.0f)

    // Map fission, pull join and transposes out
    val f7 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                MapSeq(fun( bCol =>
                  toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o MapSeq(mult) $ Zip(aRow, bCol)
                )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output7: Array[Float], _) = Execute(1, mSize)(f7, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output7, 0.0f)

    // Map fission, separate add and mult
    val f8 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                MapSeq(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f)) o
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output8: Array[Float], _) = Execute(1, mSize)(f8, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output8, 0.0f)

    // Map-Reduce interchange
    val f9 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSizeM))) o Transpose()
                  o
                  MapSeq(fun( bCol =>
                    MapSeq(mult) $ Zip(aRow, bCol)
                  )) $ bCols
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output9: Array[Float], _) = Execute(1, mSize)(f9, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output9, 0.0f)

    // Map-Map transpose, pulling the zip out
    val f10 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSizeM))) o Transpose()
                  o Transpose() o MapSeq(fun(elemRowPair =>
                  MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                  )) $ Get(elemRowPair, 1)
                )) $ Zip(aRow, Transpose() $ bCols)
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output10: Array[Float], _) = Execute(1, mSize)(f10, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output10, 0.0f)

    // Transpose() o Transpose() => id
    val f11 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(fun( aRow =>
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSizeM)))
                  o
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
              )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output11: Array[Float], _) = Execute(1, mSize)(f11, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output11, 0.0f)

    // Map fission, separate reduce
    val f12 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(
                Join () o toGlobal(MapSeq(MapSeq(id))) o
                  ReduceSeq(fun((acc, c) =>
                    MapSeq(add) $ Zip(acc, c)
                  ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSizeM)))
              )
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output12: Array[Float], _) = Execute(1, mSize)(f12, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output12, 0.0f)

    // Map fission, between reduce and copy back
    val f13 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o MapSeq(
                ReduceSeq(fun((acc, c) =>
                  MapSeq(add) $ Zip(acc, c)
                ), MapSeq(id) $ Value(0.0f, ArrayTypeWSWC(Float, chunkSizeM)))
              )
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output13: Array[Float], _) = Execute(1, mSize)(f13, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output13, 0.0f)

    // Map-Reduce interchange
    val f14 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN)))
                o Transpose()
                o
                MapSeq(fun( aRow =>
                  MapSeq(fun(elemRowPair =>
                    MapSeq(fun(bElem => mult.apply(bElem, Get(elemRowPair, 0))
                    )) $ Get(elemRowPair, 1)
                  )) $ Zip(aRow, Transpose() $ bCols)
                )) $ aRows
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output14: Array[Float], _) = Execute(1, mSize)(f14, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output14, 0.0f)

    // Map-Map interchange, pulling the zip out
    val f15 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN)))
                o Transpose()
                o Transpose() o
                MapSeq(fun( rowPair =>
                  MapSeq(fun(a =>
                    MapSeq(fun(bElem => mult.apply(bElem, a)
                    )) $ Get(rowPair, 1)
                  )) $ Get(rowPair, 0)
                )) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output15: Array[Float], _) = Execute(1, mSize)(f15, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output15, 0.0f)

    // Transpose() o Transpose() => id
    val f16 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, c) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1)))) $ Zip(acc, c)
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN)))
                o
                MapSeq(fun( rowPair =>
                  MapSeq(fun(a =>
                    MapSeq(fun(bElem => mult.apply(bElem, a)
                    )) $ Get(rowPair, 1)
                  )) $ Get(rowPair, 0)
                )) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output16: Array[Float], _) = Execute(1, mSize)(f16, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output16, 0.0f)

    // ReduceSeq-MapSeq fusion
    val f17 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    )) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output17: Array[Float], _) = Execute(1, mSize)(f17, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output17, 0.0f)

    // MapSeq(f) => toPrivate(MapSeq(f))
    val f18 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(Join () o toGlobal(MapSeq(MapSeq(id))))
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, toPrivate(MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    ))) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output18: Array[Float], _) = Execute(1, mSize)(f18, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output18, 0.0f)

    // split-join + join o split => id
    val f19 = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), N), // Already transposed
      (A, B) => {
        Map(Scatter(reorderStride(chunkSizeM))) o Join() o
          Map(TransposeW() o Join() o Map(TransposeW())) o
          MapGlb(fun( aRows =>
            MapSeq(fun( bCols =>
              MapSeq(toGlobal(MapSeq(id))) o Join()
                o
                ReduceSeq(fun((acc, rowPair) =>
                  MapSeq(fun(pair => MapSeq(add) $ Zip(Get(pair, 0), Get(pair, 1))))
                    o fun(rowPair =>
                    Zip(acc, toPrivate(MapSeq(fun(a =>
                      MapSeq(fun(bElem => mult.apply(bElem, a)
                      )) $ Get(rowPair, 1)
                    ))) $ Get(rowPair, 0))) $ rowPair
                ), MapSeq(MapSeq(id)) $ Value(0.0f, ArrayTypeWSWC(ArrayTypeWSWC(Float, chunkSizeM), chunkSizeN))
                ) $ Zip(Transpose() $ aRows, Transpose() $ bCols)
            )) o Split(chunkSizeM) o ReorderStride(chunkSizeM) $ B
          )) o Split(chunkSizeN) $ A
      })

    val (output19: Array[Float], _) = Execute(1, mSize)(f19, matrixA, transposedMatrixB)
    assertArrayEquals(gold, output19, 0.0f)
  }
}
