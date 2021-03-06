package opencl.generator.stencil

import ir.ArrayTypeWSWC
import ir.ast.{Get, Slide, Zip, fun, _}
import lift.arithmetic.SizeVar
import opencl.executor.{Execute, Executor, Compile}
import opencl.generator.stencil.acoustic.StencilUtilities
import opencl.ir._
import opencl.ir.pattern._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, _}

object TestSlideSeqPlus
{
  @BeforeClass def before(): Unit = {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after(): Unit = {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

object SlideSeqPlusHelpers
{

  def stencil(a: Int ,b :Int) = fun(
    ArrayTypeWSWC(Float, SizeVar("N")),
    (input) =>
       toGlobal(MapSeqSlide(MapSeqUnroll(id) o ReduceSeqUnroll(absAndSumUp,0.0f), a,b)) $ input
  )
}

class TestSlideSeqPlus
{

  @Test
  def reduceSlide1DTestSize3Step1(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 30
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output, _) = Execute(2,2)[Array[Float]](SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize5Step3(): Unit = {

    val slidesize = 5
    val slidestep = 3
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output, _) = Execute(2,2)[Array[Float]](SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

/*
    StencilUtilities.print1DArray(values)
    StencilUtilities.print2DArray(values.sliding(slidesize,slidestep).toArray)

    StencilUtilities.print1DArray(gold)
    StencilUtilities.print1DArray(output)
*/

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize5Step5(): Unit = {

    val slidesize = 5
    val slidestep = 5
    val size = 20
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _))

    val (output, _) = Execute(2,2)[Array[Float]](SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestSize3Step2Length10(): Unit = {

    val slidesize = 3
    val slidestep = 2
    val size = 10
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    // drop right one on the comparison array because scala sliding does not work exactly the same as Lift sliding ...
    val gold = values.sliding(slidesize,slidestep).toArray.map(x => x.reduceLeft(_ + _)).dropRight(1)

    val (output, _) = Execute(2,2)[Array[Float]](SlideSeqPlusHelpers.stencil(slidesize,slidestep), values)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Ignore
  @Test
  def reduceSlide1DTestWithWeights(): Unit = {

    val slidesize = 3
    val slidestep = 1
    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val weights = Array( 0.5f, 1.0f, 0.5f )
    val gold = Array( 4.0f,6.0f,8.0f,10.0f,12.0f,14.0f ) //values.sliding(3,1).toArray.map(x => x.reduceLeft(0.5f*_ + 0.5f*_))

    val orgStencil = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      ArrayTypeWSWC(Float, 3),
      (input,wgts) => {
        MapSeqSlide(
          fun(neighbourhood => {
            toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(add, 0.0f) o
            MapSeqUnroll(mult) $
            Zip(wgts, neighbourhood)
          }), slidesize, slidestep)
        }  $ input
    )

    val source = Compile(orgStencil)
    val (output, _) = Execute(2,2)[Array[Float]](source, orgStencil, values, weights)

    assertArrayEquals(gold, output, 0.1f)

  }

  // TODO - 2D, etc

  @Test
  def reduceSlide2DTest(): Unit = {

    val size = 8
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }

    val firstSlide = values.sliding(3,1).toArray
    val secondSlide = firstSlide.map(x => x.transpose.sliding(3,1).toArray)
    val neighbours = secondSlide.map(x => x.map(y => y.transpose))
    val gold = neighbours.map(x => x.map(y => y.flatten.reduceLeft(_ + _))).flatten


    val orgStencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3,1) $ mat
      })

    val stencil2D = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      (mat) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeq(add, 0.0f) o Join() $ neighbours
          }))
        ) o Slide2D(3,1) $ mat
      })

    val (output, _) = Execute(2,2)[Array[Float]](stencil2D, values)

    StencilUtilities.print2DArray(values)
    StencilUtilities.print1DArrayAs2DArray(output,size-2)
    StencilUtilities.print1DArray(output)
    StencilUtilities.print1DArray(gold)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide2DTestWithWeights(): Unit = {

    val size = 8
    val values = Array.tabulate(size,size) { (i,j) => (i + j + 1).toFloat }
    val weights = Array( Array(0.0f, 0.5f, 0.0f ), Array(0.5f, 1.0f, 0.5f ),Array(0.0f, 0.5f, 0.0f ) )
    val gold = Array( 9.0f,12.0f,15.0f,18.0f,21.0f,24.0f,12.0f,15.0f,18.0f,21.0f,24.0f,27.0f,15.0f,18.0f,21.0f,24.0f,27.0f,30.0f,18.0f,21.0f,24.0f,27.0f,30.0f,33.0f,21.0f,24.0f,27.0f,30.0f,33.0f,36.0f,24.0f,27.0f,30.0f,33.0f,36.0f,39.0f )


    val stencil = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, SizeVar("M")), SizeVar("N")),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, weights.length), weights.length),
      (input,wgts) => {
        MapGlb(1)(
          MapGlb(0)(fun(neighbours => {
            toGlobal(MapSeqUnroll(id)) o
              ReduceSeqUnroll(fun((acc, pair) => {
                val pixel = Get(pair, 0)
                val weight = Get(pair, 1)
                multAndSumUp.apply(acc, pixel, weight)
              }), 0.0f) $ Zip(Join() $ neighbours, Join() $ wgts)
          }))
        ) o Slide2D(3,1) $ input
      })

    val (output, _) = Execute(2,2)[Array[Float]](stencil, values, weights)

    assertArrayEquals(gold, output, 0.1f)

  }

  @Test
  def reduceSlide1DTestMapAt(): Unit = {

    val size = 8
    val values = Array.tabulate(size) { (i) => (i + 1).toFloat }
    val gold = values.sliding(3,1).toArray.map(x => x.reduceLeft(_ + _))


    val stencil = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      (input) => {
        MapGlb(
          toGlobal(MapSeqUnroll(id)) o
            ReduceSeqUnroll(fun((acc, y) => {
              absAndSumUp.apply(acc, y)
            }), 0.0f))
      } o Slide(3, 1)  $ input
    )

    val (output, _) = Execute(2,2)[Array[Float]](stencil, values)

    StencilUtilities.print1DArray(values)
    StencilUtilities.print1DArray(output)

    assertArrayEquals(gold, output, 0.1f)

  }
}
