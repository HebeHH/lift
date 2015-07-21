package opencl.generator

import apart.arithmetic.Var
import ir._
import ir.ast._
import opencl.executor.{Execute, Executor}
import opencl.ir._
import opencl.ir.ast._
import org.junit.Assert._
import org.junit.{AfterClass, BeforeClass, Test}
import opencl.ir.pattern._

object TestTriple {
  @BeforeClass def before() {
    Executor.loadLibrary()
    println("Initialize the executor")
    Executor.init()
  }

  @AfterClass def after() {
    println("Shutdown the executor")
    Executor.shutdown()
  }
}

class TestTriple {
  val inputSize = 1024
  val add3Tuple = UserFun("add3", "t", "{ return t._0+t._1+t._2; }", TupleType(Float, Float, Float), Float)
  val add3 = UserFun("add3", Array("x", "y", "z"), "{ return x+y+z; }", Seq(Float, Float, Float), Float)
  val inputArray1 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val inputArray2 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val inputArray3 = Array.fill(inputSize)(util.Random.nextInt(5).toFloat)
  val gold = (inputArray1 zip inputArray2 zip inputArray3).map({case ((a,b),c) => a+b+c})
  val inputArray = (inputArray1 zip inputArray2 zip inputArray3).map({case ((a,b),c) => Array(a,b,c)}).flatten

  @Test def VECTOR_SUM_TRIPLE() {
    val f = fun(ArrayType(TupleType(Float,Float,Float), Var("N")), (input) =>
      Join() o MapWrg(
	Join() o Barrier() o MapLcl(MapSeq(add3Tuple)) o Split(4)
      ) o Split(1024) $ input
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray)
    assertArrayEquals(gold, output, 0.0f)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)
  }

  @Test def VECTOR_SUM_TRIPLE_2() {
    val N = Var("N")
    val f = fun(
      ArrayType(Float, N),
		  ArrayType(Float, N),
      ArrayType(Float, N),
		  (xs, ys, zs) =>
        Join() o MapWrg(
  	      Join() o Barrier() o MapLcl(MapSeq(add3)) o Split(4)
        ) o Split(1024) $ Zip(xs, ys, zs)
    )

    val (output: Array[Float], runtime) = Execute(inputSize)(f, inputArray1, inputArray2, inputArray3)
    assertArrayEquals(gold, output, 0.0f)
    println("output(0) = " + output(0))
    println("runtime = " + runtime)    
  }

}
