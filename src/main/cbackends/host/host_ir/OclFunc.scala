package cbackends.host.host_ir

import core.generator.GenericAST.CTypeT
import ir.ast.{FPattern, IDGenerator, IRNode, Lambda, Param, Pattern}
import ir.interpreter.Interpreter.ValueMap
import ir.{TupleType, Type, TypeChecker}
import lift.arithmetic.ArithExpr
import opencl.generator.NDRange

import scala.collection.mutable


//accept arbitrary parameters.
case class OclFunc(override val f: Lambda,
                   val ndranges: (NDRange, NDRange) = (NDRange(1,1,1), NDRange(1,1,1)),
                   val funcName: String = "execute_" + IDGenerator.get_id(),
                   override val cpu_timer: Boolean = false,
                   override val gpu_timer: Boolean = false,
                                                   //var name, type, size, temp or not, in or out.
                   val memories: mutable.ListBuffer[(String, CTypeT, ArithExpr, Boolean, String )] = mutable.ListBuffer.empty)
  extends Pattern(arity = f.params.length) with FPattern with Measurable {

  override def copy(f: Lambda): Pattern = OclFunc(f, ndranges, funcName)


  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case tt:TupleType =>
        (f.params zip tt.elemsT).foreach( pair => pair._1.t = pair._2  )
      case _ =>
        f.params(0).t = argType
    }
    TypeChecker.check(f.body, setType)
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[_] = {
    assert(args.length == arity)
    args.head match {
      case a: Vector[_] => a.map(f.eval(valueMap, _))
    }
  }
}
case class OclFunCall(funcName : String, params: Array[Param],
                      val ndranges: (NDRange, NDRange) = (NDRange(1,1,1), NDRange(1,1,1)),
                      override val cpu_timer: Boolean = false, override val gpu_timer: Boolean = false)
  extends Pattern(arity = 2) with Measurable {

  override def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode): IRNode = this
  override def checkType(argType: Type, setType: Boolean): Type = {
    assert(false, "OclFunCall* never do type check")
    argType
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = this

}
