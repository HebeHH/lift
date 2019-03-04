package rewriting.rules

import ir.ast.FunCall
import ir.ast.onnx.{AveragePool, ConvWithBias, ConvWithoutBias}
import patterns.nn.conv.{ConvCPU3D, ConvStencil3D}
import patterns.nn.pool.PoolCPU3D

/**
  * Neural Network-specific rewriting rules
  */
object NeuralNetRules {

  /*** Per-frontend lowering rules ***/
  object ONNXLoweringRules {
    /** Convolution **/
    // This rule only works for convolutions with the stride of 1
    val convWithoutBiasAsCPUFunc = Rule("ConvWithoutBias => <ConvCPU3D expression>", {
      case call @ FunCall(_: ConvWithoutBias, args @ _*) if args.length == 2 =>

        ConvCPU3D(call, call.args.head, call.args(1))
    })

    val convWithBiasAsStencil = Rule("ConvWithBias => <ConvStencil3D expression>", {
      case call @ FunCall(onnxNode: ConvWithBias, args @ _*) if args.length == 3 =>
        ConvStencil3D(
          layerConfig = ConvStencil3D.LayerConfig(onnxNode, args.head),
          optParams = ConvStencil3D.OptParams(),
          input = args.head, weights = args(1), biases = args(2))
    })


    /** Pooling **/
    val averagePoolAsCPUFunc = Rule("AveragePool => <PoolCPU3D expression>", {
      case call @ FunCall(_: AveragePool, args @ _*) if args.length == 1 =>

        PoolCPU3D(call, call.args.head)
    })
  }
}
