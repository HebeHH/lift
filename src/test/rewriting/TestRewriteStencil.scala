package rewriting

import ir._
import ir.ast._
import opencl.executor._
import opencl.ir._
import opencl.ir.pattern.ReduceSeq
import org.junit.Assert._
import org.junit.Test
import rewriting.rules._
import rewriting.macrorules.{EnablingRules, MacroRules, SlideTiling}
import rewriting.utils.NumberExpression

object TestRewriteStencil extends TestWithExecutor

class TestRewriteStencil {

  @Test
  def stencil2DTiling(): Unit = {
    val M = 128
    val N = 128
    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N),
      (input) =>
        //MapGlb(1)(MapGlb(0)(toGlobal(MapSeq(id)) o ReduceSeq(add, 0.0f) o Join())) o Slide2D(3,1) o Pad2D(1,1,Pad.Boundary.Clamp) $ input
        Map(Map(ReduceSeq(add, 0.0f) o Join())) o Slide2D(3,1) o Pad2D(1,1,Pad.Boundary.Clamp) $ input
    )

    val weights = Array.tabulate(9)(x => 1.0f) // just required for gold computation
    val A = Array.tabulate(M,N)((x,y) => util.Random.nextInt(500).toFloat)

    // TODO move to Utils
    val scalaClamp = (idx: Int, length: Int) => {
      if(idx<0) 0 else if(idx>length-1) length-1 else idx
    }

    val gold: Array[Float] = Utils.scalaCompute2DStencil(A,3,1,3,1,1,1,1,1,weights,scalaClamp)

    val f1 = Rewrite.applyRuleAtId(f, 2, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 1, EnablingRules.movingJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 11, Rules.slideTiling(4))
    val f4 = Rewrite.applyRuleAtId(f3, 5, FissionRules.mapFission)
    val f5 = Rewrite.applyRuleAtId(f4, 6, FissionRules.mapFission)
    val f6 = Rewrite.applyRuleAtId(f5, 4, Rules.slidePromotion)
    val f7 = Rewrite.applyRuleAtId(f6, 3, FusionRules.mapFusion)
    val f8 = Rewrite.applyRuleAtId(f7, 18, Rules.slidePromotion)
    val f9 = Rewrite.applyRuleAtId(f8, 3, FissionRules.mapFission)
    val f10 = Rewrite.applyRuleAtId(f9, 5, Rules.slideSwap)
    val f11 = Rewrite.applyRuleAtId(f10, 5, CopyRules.addId)
    val f12 = Rewrite.applyRuleAtId(f11, 6, CopyRules.implementOneLevelOfId)
    val f13 = Rewrite.applyRuleAtId(f12, 16, Rules.idTransposeTranspose)
    val f14 = Rewrite.applyRuleAtId(f13, 6, FissionRules.mapFission)
    val f15 = Rewrite.applyRuleAtId(f14, 5, Rules.slideTransposeSwap)
    val f16 = Rewrite.applyRuleAtId(f15, 4, Rules.slideTransposeReordering)
    //
    val f17 = Rewrite.applyRuleAtId(f16, 2, FusionRules.mapFusion)
    val f18 = Rewrite.applyRuleAtId(f17, 35, FusionRules.mapFusion)
    val f19 = Rewrite.applyRuleAtId(f18, 37, Rules.transposeMapJoinReordering)
    val f20 = Rewrite.applyRuleAtId(f19, 35, FissionRules.mapFission)
    val f21 = Rewrite.applyRuleAtId(f20, 2, FissionRules.mapFission)
    val f22 = Rewrite.applyRuleAtId(f21, 36, FissionRules.mapFission)
    val f23 = Rewrite.applyRuleAtId(f22, 3, FissionRules.mapFission)
    // cancel **T o **T
    val f24 = Rewrite.applyRuleAtId(f23, 4, FusionRules.mapFusion)
    val f25 = Rewrite.applyRuleAtId(f24, 32, FusionRules.mapFusion)
    val f26 = Rewrite.applyRuleAtId(f25, 34, SimplificationRules.transposeTransposeId2)
    val f27 = Rewrite.applyRuleAtId(f26, 4, SimplificationRules.dropId)
    //
    val f28 = Rewrite.applyRuleAtId(f27, 1, Rules.joinSwap)
    val f29 = Rewrite.applyRuleAtId(f28, 3, Rules.transposeSwap)
    // todo store result in global memory to make kernel executable
    //println(NumberExpression.breadthFirst(f29.body).mkString("\n\n"))
    //println("@@@@@@@@@ EXPRESSION:\n" + f29)

    //val(result: Array[Float], _) = Execute(M,N)(f29,A)
    //assertArrayEquals(gold, result, 0.001f)
    TypeChecker(f29)
  }

  @Test
  def stencil1DTiling(): Unit = {
    val n = 128
    val f = fun(
      ArrayTypeWSWC(Float, n),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, EnablingRules.movingJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, FusionRules.mapFusion)
    // introduce low-level primitives
    val f4 = Rewrite.applyRuleAtId(f3, 8, OpenCLRules.reduceSeq)
    val f5 = Rewrite.applyRuleAtId(f4, 1, OpenCLRules.mapWrg)
    val f6 = Rewrite.applyRuleAtId(f5, 5, OpenCLRules.mapLcl)
    // copy result back to global memory
    val f7 = Rewrite.applyRuleAtId(f6, 8, CopyRules.addIdAfterReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 15, CopyRules.implementIdAsDeepCopy)
    val f9 = Rewrite.applyRuleAtId(f8, 8, OpenCLRules.globalMemory)

    val (result, _) = Execute(n)[Array[Float]](f9, A)
    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def stencil1DTilingLocalMemory(): Unit = {
    val n = 128
    val f = fun(
      ArrayTypeWSWC(Float, n),
      (input) =>
        Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    // tiling
    val f1 = Rewrite.applyRuleAtId(f, 1, Rules.slideTiling(4))
    val f2 = Rewrite.applyRuleAtId(f1, 0, EnablingRules.movingJoin)
    val f3 = Rewrite.applyRuleAtId(f2, 1, FusionRules.mapFusion)
    // local memory
    val f4 = Rewrite.applyRuleAtId(f3, 6, CopyRules.addId)
    val f5 = Rewrite.applyRuleAtId(f4, 7, CopyRules.implementIdAsDeepCopy)
    val f6 = Rewrite.applyRuleAtId(f5, 11, OpenCLRules.reduceSeq)
    val f7 = Rewrite.applyRuleAtId(f6, 11, CopyRules.addIdAfterReduce)
    val f8 = Rewrite.applyRuleAtId(f7, 18, CopyRules.implementIdAsDeepCopy)
    val f9 = Rewrite.applyRuleAtId(f8, 1, OpenCLRules.mapWrg)
    val f10 = Rewrite.applyRuleAtId(f9, 5, OpenCLRules.mapLcl)
    val f11 = Rewrite.applyRuleAtId(f10, 7, OpenCLRules.mapLcl)
    val f12 = Rewrite.applyRuleAtId(f11, 7, OpenCLRules.localMemory)
    val f13 = Rewrite.applyRuleAtId(f12, 13, OpenCLRules.globalMemory)
    //val test = NumberExpression.breadthFirst(f13.body)

    val (result, _) = Execute(n)[Array[Float]](f13, A)
    assertArrayEquals(gold, result, 0.001f)
  }

  @Test
  def stencil1DMacroRule(): Unit = {
    val n = 128
    val f = fun(
      ArrayTypeWSWC(Float, n),
      (input) =>
        Join() o Map(Reduce(add, 0.0f)) o Slide(3,1) o Pad(1,1,Pad.Boundary.Clamp) $ input
      )

    val f1 = Rewrite.applyRuleAtId(f, 1, SlideTiling.tileStencils)
    val f2 = Rewrite.applyRuleAtId(f1, 9, OpenCLRules.reduceSeq)
    val f3 = Rewrite.applyRuleAtId(f2, 9, CopyRules.addIdAfterReduce)
    val f4 = Rewrite.applyRuleAtId(f3, 16, CopyRules.implementIdAsDeepCopy)
    val f5 = Rewrite.applyRuleAtId(f4, 9, OpenCLRules.globalMemory)
    val f6 = Rewrite.applyRuleAtId(f5, 2, OpenCLRules.mapWrg)
    val f7 = Rewrite.applyRuleAtId(f6, 6, OpenCLRules.mapLcl)
    val test = NumberExpression.breadthFirst(f5.body)
    println(test.mkString("\n\n"))

    val A = Array.fill(n)(util.Random.nextInt(500).toFloat)
    val gold = (A.slice(0,1) ++ A ++ A.slice(n-1,n)).sliding(3,1).map(x =>
      x.sum).toArray

    TypeChecker(f7)
    // kernel contains generic tile size as additional kernel argument
    val kernel = Compile(f7)
  }
}
