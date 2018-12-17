package cbackends.common.utils.output_view

import ir.ast.{Expr, Lambda, Param}
import ir.view.{NoView, UnusedInExprOutputView, ViewMem}

import scala.collection.mutable

object OutputView {

  def pre_check(lambda: Lambda) : Unit = {

    lambda visitBy {
      case e:Expr => assert(e.outputView == NoView)
      case _ =>
    }

  }

  def post_check(lambda: Lambda) : Unit = {

    //If some params are not used in expression,
    //set their outputView explicitly to avoid NoView assertion failure
    val all_params = lambda.params.toSet
    val used_params = mutable.Set.empty[Param]
    lambda.body visitBy {
      case p:Param if all_params contains p => used_params += p
      case _ =>
    }
    val used_params_immutable = used_params.toSet
    val unused_params = all_params -- used_params_immutable
    unused_params.foreach(p => p.outputView = UnusedInExprOutputView)

    lambda visitBy {
      case e:Expr => assert( e.outputView != NoView )
      case _ =>
    }

  }

  def init_params(lambda: Lambda) : Unit = {

    //first set the body's output view, then propagate to someone inside.
    lambda.body.outputView = ViewMem(lambda.body.mem.variable, lambda.body.t)

  }

}
