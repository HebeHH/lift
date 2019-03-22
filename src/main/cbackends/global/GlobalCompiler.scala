package cbackends.global

import cbackends.common.CBackendsCompilerTrait
import cbackends.common.common_cast.CbackendCAST.SourceFile
import cbackends.global.analysis.OclKernelFileNameAnalysis
import cbackends.global.lowering.{GenerateCLockPrintingStmt, GenerateGlobalClockDecl, GenerateOclGlobalFacility}
import cbackends.global.transformation.cast_transformation.cpu_outline_transformation.{CPUOutlineTargetAnalysis, OclOutlineTargetAnalysis}
import cbackends.global.transformation.empty_kernel_structure.EmptyKernelStructure
import cbackends.global.transformation.funcall2closure.FunCall2Closure
import cbackends.global.transformation.ocl_memory_gen.OclMemoryGen
import cbackends.global.transformation.purify.Purify
import cbackends.global.transformation.unique_user_func.UniqueUserFunc
import cbackends.host.HostCompiler
import cbackends.host.lowering.LowerIR2HostCAST
import core.generator.GenericAST
import core.generator.GenericAST.{Block, CVarWithType, ExpressionStatement, FunctionCall, FunctionPure, RawCode, StringConstant, VoidType}
import ir.ast.{FunCall, Lambda}
import lift.arithmetic.ArithExpr

object GlobalCompiler{

  def ! (lambda: Lambda, path: String, files: List[String]): Unit = {


    assert(files.length == 1, "There should be exactly one file name passed")


    HostCompiler.typeCheck(lambda)

    val all_cpufunc_outline_targets = CPUOutlineTargetAnalysis(lambda)

    val all_oclfunc_outline_targets = OclOutlineTargetAnalysis(lambda)

    val emptified_lambda = EmptyKernelStructure(lambda)

    //-----------------------------------------------------------------------//
    //DO ALL IR TRANSFORMATION ABOVE, FROM HERE, NO IR TRANSFORMATION ALLOWED
    //But CAST transformation are OK below
    //-----------------------------------------------------------------------//

    val cpufundefs = all_cpufunc_outline_targets.map( HostCompiler.!!(_, false) )
    val final_cpufundefs = UniqueUserFunc(cpufundefs)

    all_oclfunc_outline_targets.size match {
      //CPU only
      case 0 => {

        val top_cast = HostCompiler !! emptified_lambda
        HostCompiler.castPrinter(List(  new SourceFile(path, files(0), Block(Vector(LowerIR2HostCAST.boilerplate_code), global = true)  :+ ( Block( final_cpufundefs.toVector, global = true) :: top_cast  )) ) )

      }
      case _ => {

        val all_oclfunc_outline_targets_purified = all_oclfunc_outline_targets.map{
          case (filename:String, lambdax:Lambda) => (filename, Purify(lambdax) )
        }
        val oclfundefs = all_oclfunc_outline_targets_purified.map {
          case (filename:String, lambdax:Lambda) =>  (filename , ( opencl.executor.Compile.!!(lambdax) ) )
        }

        OclMemoryGen(lambda)

        //val final_oclfundefs = UniqueUserFunc(oclfundefs)
        val final_oclfundefs = oclfundefs

        //val ocl_kernel_file_names = OclKernelFileNameAnalysis(emptified_lambda)
        val (global_val_decl_cast, global_val_init_cast) = GenerateOclGlobalFacility(emptified_lambda, path)

        val global_clock_decl = GenerateGlobalClockDecl(emptified_lambda)
        val clock_printing_stmt = GenerateCLockPrintingStmt(emptified_lambda)
        val final_global_var_decl = global_val_decl_cast :++ global_clock_decl

        val print_csv_header = ExpressionStatement(StringConstant("std::cerr<<"+'"'+ "func_name, cpu_time_ms, gpu_time_ms, diff_percentage"+'"'+"<<std::endl" ) )
        val clock_printing_boilerplates = RawCode(
          """
            |double cpu_time_in_ms( std::chrono::milliseconds start, std::chrono::milliseconds finish ){
            | return (finish - start).count();
            |}
            |
            |double gpu_time_in_ms( cl::Event event ){
            |
            |  cl_ulong start;
            |  cl_ulong end;
            |  cl_int err;
            |
            |  event.wait();
            |
            |  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_START,
            |                                sizeof(start), &start, NULL);
            |  assert(err == CL_SUCCESS);
            |
            |  err = clGetEventProfilingInfo(event(), CL_PROFILING_COMMAND_END,
            |                                sizeof(end), &end, NULL);
            |  assert(err == CL_SUCCESS);
            |
            |  return ((double)(end - start)) * 1.0e-6;
            |}
            |
            |double diff_percent(double lhs, double rhs) {
            |  if(std::min(lhs,rhs)==0)
            |    return -1;
            |  else
            |    return std::abs(lhs-rhs)/std::min(lhs,rhs);
            |}
            |
          """.stripMargin )
        val clock_printing_function = FunctionPure("print_clock", VoidType(), List(), print_csv_header +: clock_printing_stmt)
        val post_execute_hook = FunctionPure("post_execute", VoidType(), List(), Block( Vector(
          if(global_clock_decl.content.length > 0 ) FunctionCall("print_clock", List() ) else RawCode("")
        ), global = true
        ))
        val final_global_func_decl = global_val_init_cast :+ clock_printing_boilerplates :+ clock_printing_function :+ post_execute_hook

        val top_cast = HostCompiler !! (emptified_lambda, true)

        HostCompiler.castPrinter(List(  new SourceFile(path, files(0), Block(Vector(LowerIR2HostCAST.boilerplate_code, LowerIR2HostCAST.ocl_boilerplate_code), global = true) :+ final_global_var_decl :+ final_global_func_decl :+ ( Block( final_cpufundefs.toVector, global = true) :: top_cast  )) ) )

        val ocl_source_files = final_oclfundefs.map{ case (fileName,block) => new SourceFile(path,fileName,block) }.toList
        HostCompiler.castPrinter(ocl_source_files)
      }
    }


    println("hello")

  }


}
