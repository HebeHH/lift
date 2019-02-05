package cbackends.common.executor

import scala.sys.process._

object Executor {

  private def compile_native(path: String, file: String): Unit = {

    val full_path_file = path + "/" + file
    val target = path + "/" + "a.out"

    val status_code = s"g++ $full_path_file -I$path -o $target" !

    assert(status_code == 0, "Native Compilation error!")


  }

  private def run_executable(path: String, file: String): String = {

    ( ( if(path.isEmpty()) "./" else path + "/" ) + s"$file" ) !!

  }

  def native_compile_and_run(path: String, file: String)  : String = {

    compile_native(path, "main.cpp")

    val status_code = (s"rm $path" + "/" + s"$file") !

    assert(status_code == 0, "Delete generated lib file error!")

    val result = run_executable(path, "a.out")

    val status_code2 = (s"rm $path" + "/a.out") !

    assert(status_code2 == 0, "Delete generated binary error!")

    result


  }
}
