package cbackends.common.memory_management

import core.generator.GenericAST.CVarWithType
import ir.ast.{AbstractMap, AbstractPartRed, Array3DFromUserFunGenerator, ArrayAccess, ArrayFromUserFunGenerator, FunCall, Get, IRNode, Iterate, Join, Lambda, Pad, Param, Slide, Split, Transpose, TransposeW, UserFun, Zip}
import lift.arithmetic.ArithExpr
import cbackends.common.utils.type_lowering.TypeLowering
import cbackends.host.host_ir._
import cbackends.sdh.sdh_ir.{TMKernel, ToGPE, ToLCP}
import ir.Type
import opencl.ir.{CollectTypedOpenCLMemory, OpenCLAddressSpace}
import opencl.ir.pattern.ScanSeq

import scala.collection.mutable

object FinalMemoryAllocationAnalysis {

  //var hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ]

  def analyze(node:IRNode) : Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)] = {
    node match {

      case _:Param | _:ArrayFromUserFunGenerator | _:Array3DFromUserFunGenerator => Map.empty

      case fc@FunCall(_:UserFun | _:CPUFunContainer | _:OclFunContainer | _:ToGPU | _:ToHost | _:TMKernel, args@_*) =>
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
        val mem_of_args_input_and_output = args_map + (
          fc.mem.variable.toString -> (
            CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ),
            Type.getElementCount(fc.t),
            fc.addressSpace
          ) )

          val intermediate_global_mem: Seq[(String, (CVarWithType, ArithExpr, OpenCLAddressSpace))] = (fc.f match {
            case cfc: CPUFunContainer => cfc.intermediateGlobalMem
            case ofc: OclFunContainer => ofc.intermediateGlobalMem
            case _ => Seq()
          }).map(buf =>
            buf.mem.variable.toString -> (
              CVarWithType(buf.mem.variable.toString,
                TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(buf.t), true ) ),
              Type.getElementCount(buf.t),
              buf.mem.addressSpace))

          mem_of_args_input_and_output ++ intermediate_global_mem


          val intermediate_global_mem: Seq[(String, (CVarWithType, ArithExpr, OpenCLAddressSpace))] = (fc.f match {
            case cfc: CPUFunContainer => cfc.intermediateGlobalMem
            case ofc: OclFunContainer => ofc.intermediateGlobalMem
            case _ => Seq()
          }).map(buf =>
            buf.mem.variable.toString -> (
              CVarWithType(buf.mem.variable.toString,
                TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(buf.t), true ) ),
              Type.getElementCount(buf.t),
              buf.mem.addressSpace))

          mem_of_args_input_and_output ++ intermediate_global_mem


      case fc@FunCall(r:AbstractPartRed, args@_*) =>
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
        val args_body_map = args_map ++ analyze(r.f.body)
        //correct type for user function, e.g., float => [float]_1
         args_body_map + (
          fc.mem.variable.toString -> (
            CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) ,
            Type.getElementCount(fc.t),
            fc.addressSpace
          ) )


      case fc@FunCall(s:ScanSeq, args@_*) =>
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
        val args_body_map = args_map ++ analyze(s.f.body)
        //correct type for user function, e.g., float => [float]_1
        args_body_map + (
          fc.mem.variable.toString -> (
            CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) ,
            Type.getElementCount(fc.t),
            fc.addressSpace
          ) )

      case FunCall(_:ir.ast.Map, args@_*) =>
        // no memory correction for Map, which is a lazy struct
        // no need to traverse into Map, also because it is a lazy struct, no memory alloc can happen, thus no need for correction
        // args.foreach(analyze(_))
        args.map(analyze(_)).reduce( _ ++ _ )

      case fc@FunCall(m:AbstractMap, args@_*) =>
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
        val args_body_map = args_map ++ analyze(m.f.body)
        //correct type for user function, e.g., float => [float]_1
        args_body_map + (
          fc.mem.variable.toString -> (
            CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) ,
            Type.getElementCount(fc.t),
            fc.addressSpace
          ) )
        /*
        args.foreach(analyze(_))
        analyze(m.f.body)
        //here fc.t already have the augmented size information after map, so no need to manually calculate
        hostMemoryDeclaredInSignature +=
          fc.mem.variable.toString -> (
            CVarWithType(fc.mem.variable.toString, TypeLowering.Array2Pointer( TypeLowering.IRType2CastType(fc.t), true ) ) ,
            Type.getElementCount(fc.t),
            fc.addressSpace
          )*/


      case fc@FunCall(i:Iterate, args@_*) =>
        //args.foreach(analyze(_))
        //analyze(i.f.body)
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
       args_map ++ analyze(i.f.body)

      case fc@FunCall(l:Lambda, args@_*) =>
        //args.foreach(analyze(_))
        //analyze(l.body)
        val args_map = args.map(analyze(_)).reduce( _ ++ _ )
        args_map ++ analyze(l.body)

      case fc@FunCall(_:Join|_:Slide|_:Zip|_:Get|_:Split|_:Join|_:Transpose|_:TransposeW|_:Pad |_:ToLCP | _:ToGPE | _:ArrayAccess, args@_*) =>
        //args.foreach(analyze(_))
        args.map(analyze(_)).reduce( _ ++ _ )

      /*case fc@FunCall(_, args@_*) =>
        assert(false)

      case _ => */

    }
  }

  def apply(lambda: Lambda): Map[String, (CVarWithType, ArithExpr, OpenCLAddressSpace) ] = {

    //reset hostMemoryDeclaredInSignature if run with multiple test cases
    //hostMemoryDeclaredInSignature = mutable.Map.empty[String, (CVarWithType, ArithExpr, OpenCLAddressSpace)]

    analyze(lambda.body)

    //hostMemoryDeclaredInSignature.toMap

  }

}
