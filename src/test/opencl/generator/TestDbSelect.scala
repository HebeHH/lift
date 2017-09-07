package opencl.generator

import benchmarks.DbSelect
import opencl.executor.{Execute, TestWithExecutor}
import org.junit.Assert.assertArrayEquals
import org.junit.Test

object TestDbSelect extends TestWithExecutor

class TestDbSelect {
  // As long as junit is not able to compare of objects of type
  // Array[(Int, Int, Int)], we flatten them.
  private def goldSelect(colA: Array[Int],
                         colB: Array[Int],
                         colC: Array[Int]): Array[Int] =
    Array(
      colC.map(n => if (n == 1) 1 else 0),
      colA,
      colB
    ).transpose.flatten
  
  @Test def testNaive(): Unit = {
    val inputSize = 1024
    val colA = Array.fill(inputSize)(util.Random.nextInt(5))
    val colB = Array.fill(inputSize)(util.Random.nextInt(5))
    val colC = Array.fill(inputSize)(util.Random.nextInt(5))
  
    val (output: Array[Int], runtime) = Execute(inputSize)(
      DbSelect.naive, colA, colB, colC
    )
   
    println(s"Runtime: $runtime")
    
    assertArrayEquals(goldSelect(colA, colB, colC), output)
  }
  
  @Test def testDivideNConquer(): Unit = {
    val inputSize = 128*128
    val colA = Array.fill(inputSize)(util.Random.nextInt(5))
    val colB = Array.fill(inputSize)(util.Random.nextInt(5))
    val colC = Array.fill(inputSize)(util.Random.nextInt(5))
  
    val (output: Array[Int], runtime) = Execute(inputSize)(
      DbSelect.divideNConquer, colA, colB, colC
    )
  
    println(s"Runtime: $runtime")
  
    assertArrayEquals(goldSelect(colA, colB, colC), output)
  }
}
