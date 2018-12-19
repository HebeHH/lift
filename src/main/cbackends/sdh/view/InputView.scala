package cbackends.sdh.view

import cbackends.sdh.sdh_ir.{MapGPESync, TMKernel, ToGPE, ToLCP}
import cbackends.common.utils.input_view.InputView.{init_params, post_check, pre_check}
import cbackends.common.utils.pattern_matching.IsDefinedAt
import ir.ast.{FunCall, IRNode, Lambda}

object InputView {

  def generateInputView(node: IRNode , cont: IRNode => IRNode ) : IRNode = {
    node match {

      case fc@FunCall(_:ToGPE, arg)  => {
        cont( arg )
        fc.view = arg.view
        fc
      }
      case fc@FunCall(_:ToLCP, arg)  => {
        cont( arg )
        fc.view = arg.view
        fc
      }

      case fc@FunCall(_:MapGPESync, arg)  => {
        cont( arg )
        fc.view = arg.view
        fc
      }
      case fc@FunCall(tm:TMKernel, arg)  => {
        cont( arg)

        tm.f.params.head.view = arg.view
        cont( tm.f.body )

        fc.view = arg.view

        fc
      }

    }
  }

  def composed_generateInputView(in: IRNode) : IRNode = {

    /*
    val partial_binded_common = cbackends.common.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val partial_binded_sdh = cbackends.sdh.view.InputView.generateInputView(_:Option[IRNode], composed_generateInputView)
    val composed = partial_binded_common andThen partial_binded_sdh andThen cbackends.common.utils.pattern_matching.Error.error[IRNode] _
    composed(in)
    */
    val partial_binded_common = new PartialFunction[IRNode, IRNode] with IsDefinedAt[IRNode]
      { def apply(x: IRNode) = cbackends.common.view.InputView.generateInputView(x, composed_generateInputView) }
    val partial_binded_sdh = new PartialFunction[IRNode,IRNode] with IsDefinedAt[IRNode]
      { def apply(x: IRNode) = cbackends.sdh.view.InputView.generateInputView(x, composed_generateInputView) }
    val composed = partial_binded_common orElse partial_binded_sdh
    composed(in)

  }


  def apply(lambda: Lambda): Unit = {

    pre_check(lambda)

    init_params(lambda)

    composed_generateInputView( lambda.body )

    post_check(lambda)


  }


}
