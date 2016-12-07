package ir.view

import lift.arithmetic.{ArithExpr, Cst}
import ir._
import ir.ast._
import opencl.ir.{LocalMemory, OpenCLMemory, PrivateMemory}
import opencl.ir.pattern._

/**
 * Helper object for building views.
 *
 * Determine the dimensionality and length of each dimension of all global,
 * local and private arrays, as well as the iteration/access variables in all
 * dimensions.
 *
 * Needs to be done separately for input and output, as an e.g. expression can read
 * from global memory and write to private.
 *
 * The tuples contain the variable in the first position and the length in
 * the second.
 *
 */
object BuildDepthInfo {

  /**
   * Determine the dimensions, variables and lengths.
   *
   * @param expr Starting expression.
   */
  def apply(expr: Expr): Unit = (new BuildDepthInfo).visitAndBuildDepthInfo(expr)
}

class AccessInfo(var privateAccessInf: List[(ArithExpr, ArithExpr)],
                 var localAccessInf: List[(ArithExpr, ArithExpr)],
                 var globalAccessInf: List[(ArithExpr, ArithExpr)],
                 var l: Seq[AccessInfo]) {

  def apply(thisLevel: (ArithExpr, ArithExpr),
            usePrivate: Boolean, useLocal: Boolean): AccessInfo = {

    if (l.isEmpty) {
      val privateAccess = if (usePrivate) thisLevel :: privateAccessInf else privateAccessInf
      val localAccess = if (useLocal) thisLevel :: localAccessInf else localAccessInf
      val globalAccess = thisLevel :: globalAccessInf
      AccessInfo(privateAccess, localAccess, globalAccess)
    } else {
      AccessInfo(l.map(_(thisLevel, usePrivate, useLocal)))
    }
  }

  override def toString = s"AccessInfo($privateAccessInf, $localAccessInf, $globalAccessInf, $l)"
}

object AccessInfo {
  def apply() = new AccessInfo(List(), List(), List(), List())

  def apply(privateInf: List[(ArithExpr, ArithExpr)],
            localInf: List[(ArithExpr, ArithExpr)],
            globalInf: List[(ArithExpr, ArithExpr)]) =
    new AccessInfo(privateInf, localInf, globalInf, List())

  def apply(l: Seq[AccessInfo]) =
    new AccessInfo(List(), List(), List(), l)
}

private class BuildDepthInfo() {
  var privateAccessInf = List[(ArithExpr, ArithExpr)]()
  var localAccessInf = List[(ArithExpr, ArithExpr)]()
  var globalAccessInf = List[(ArithExpr, ArithExpr)]()

  var seenMapLcl = false

  private def visitAndBuildDepthInfo(expr: Expr): AccessInfo = {
    val result = expr match {
      case call: FunCall => buildDepthInfoFunCall(call)
      case p: Param =>
        p.inputDepth = getAccessInf(p.addressSpace.containsAddressSpace(PrivateMemory),
          p.addressSpace.containsAddressSpace(LocalMemory))
        p.accessInf
    }

    expr.accessInf = result
    result
  }

  private def buildDepthInfoFunCall(call: FunCall): AccessInfo = {

    val argInf = buildDepthForArgs(call)

    val result = call.f match {
      case m: AbstractMap => buildDepthInfoMapCall(m, call, argInf)
      case r: AbstractPartRed => buildDepthInfoReduceCall(r, call, argInf)
      case _ =>

        val (readsLocal, readsPrivate) = readsLocalPrivate(call)
        val (writesLocal, writesPrivate) = writesLocalPrivate(call)

        setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

        call.f match {
          case l: Lambda => buildDepthInfoLambda(l, call, argInf)
          case fp: FPattern => buildDepthInfoLambda(fp.f, call, argInf)
          case Get(n) => if (argInf.l.nonEmpty) argInf.l(n) else argInf
          case _: UserFun =>
            AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
          case _ => argInf
        }
    }

    result
  }

  private def buildDepthInfoMapCall(m: AbstractMap, call: FunCall,
                                    l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = readsLocalPrivate(call)

    val orig = seenMapLcl

    if (m.isInstanceOf[MapLcl])
    seenMapLcl = true

    m.f.params.head.accessInf =
      l((Type.getLength(call.args.head.t), m.loopVar), readsPrivate, readsLocal || seenMapLcl)
    buildDepthInfoPatternCall(m.f.body, call, m.loopVar, readsLocal, readsPrivate)

    if (m.isInstanceOf[MapLcl])
      seenMapLcl = orig

    if (m.f.body.isConcrete) // create fresh input view for following function
      AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
    else // call.isAbstract, return input
      l
  }

  private def readsLocalPrivate(call: FunCall) = containsLocalPrivate(call.args.head.mem)

  private def writesLocalPrivate(call: FunCall) = containsLocalPrivate(call.mem)

  private def containsLocalPrivate(mem: Memory) = {
    val containsLocal = OpenCLMemory.containsLocalMemory(mem)
    val containsPrivate = OpenCLMemory.containsPrivateMemory(mem)
    (containsLocal, containsPrivate)
  }

  private def buildDepthInfoReduceCall(r: AbstractPartRed, call: FunCall,
                                       l: AccessInfo): AccessInfo = {

    val (readsLocal, readsPrivate) = containsLocalPrivate(call.args(1).mem)
    val length = Type.getLength(call.args(1).t)

    r.f.params(0).accessInf = l.l.head
    r.f.params(1).accessInf = l.l(1)((length, r.loopVar), readsPrivate, readsLocal || seenMapLcl)

    buildDepthInfoReducePatternCall(r.f.body, call, Cst(0), r.loopVar, readsLocal, readsPrivate, l)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def buildDepthInfoReducePatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                              index2: ArithExpr,
                                              readsLocal: Boolean, readsPrivate: Boolean,
                                              l: AccessInfo
                                             ): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    updateAccessInf(readsLocal, readsPrivate, tuple, writesLocal, writesPrivate)
    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readsLocal, readsPrivate, writesLocal, writesPrivate)

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

  }

  private def updateAccessInf(readsLocal: Boolean, readsPrivate: Boolean, tuple: (ArithExpr, ArithExpr), writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    globalAccessInf = tuple :: globalAccessInf
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = tuple :: localAccessInf
    if (readsPrivate || writesPrivate)
      privateAccessInf = tuple :: privateAccessInf
  }

  private def buildDepthInfoPatternCall(expr: Expr, call: FunCall, index: ArithExpr,
                                        readsLocal: Boolean, readsPrivate: Boolean): Unit = {
    val tuple = (Type.getLength(call.t), index)
    val (writesLocal, writesPrivate) = writesLocalPrivate(call)

    updateAccessInf(readsLocal, readsPrivate, tuple, writesLocal, writesPrivate)

    // traverse into call.f
    visitAndBuildDepthInfo(expr)

    restoreAccessInf(readsLocal, readsPrivate, writesLocal, writesPrivate)

    setDepths(call, readsLocal, readsPrivate, writesLocal, writesPrivate)

    AccessInfo(privateAccessInf, localAccessInf, globalAccessInf)
  }

  private def restoreAccessInf(readsLocal: Boolean, readsPrivate: Boolean, writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    globalAccessInf = globalAccessInf.tail
    if (seenMapLcl || readsLocal || writesLocal)
      localAccessInf = localAccessInf.tail
    if (readsPrivate || writesPrivate)
      privateAccessInf = privateAccessInf.tail
  }

  private def setDepths(call: FunCall, readsLocal: Boolean, readsPrivate: Boolean,
                        writesLocal: Boolean, writesPrivate: Boolean): Unit = {
    call.inputDepth = getAccessInf(writesPrivate, writesLocal)
    call.outputDepth = getAccessInf(readsPrivate, readsLocal)
  }

  private def buildDepthForArgs(call: FunCall): AccessInfo = {

    if (call.args.length == 1)
      visitAndBuildDepthInfo(call.args.head)
    else
      AccessInfo(call.args.map((expr: Expr) => visitAndBuildDepthInfo(expr)))
  }

  private def buildDepthInfoLambda(l: Lambda, call: FunCall,
                                   list: AccessInfo): AccessInfo = {

    if (call.args.length == 1)
      setAccessInfo(list, l.params.head)
    else
      (l.params, list.l).zipped.foreach((param, accessInfo) => setAccessInfo(_, _))

    visitAndBuildDepthInfo(l.body)
  }

  private def setAccessInfo(list: AccessInfo, param: Param): Unit = {
    val (readsLocal, readsPrivate) = containsLocalPrivate(param.mem)

    param.outputDepth = getAccessInf(readsPrivate, readsLocal)
    param.accessInf = list
  }

  private def getAccessInf(readsPrivate: Boolean, readsLocal: Boolean): List[(ArithExpr, ArithExpr)] = {
    if (readsPrivate)
      privateAccessInf
    else if (readsLocal)
      localAccessInf
    else
      globalAccessInf
  }
}
