package opencl.ir

package object ast {

  val dot = OpenCLBuiltInFun("dot", Seq(Float4, Float4), Float)

  val fma = OpenCLBuiltInFun("fma", Seq(Float, Float, Float), Float)

}
