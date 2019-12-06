package ir.printer

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, _}

import opencl.ir.abs

object PrinterRunner {
  val N = SizeVar("N")
  val M = SizeVar("M")
  val clamp = Pad.Boundary.Clamp

  val lowLevelUnrolled = λ(
    ArrayType(Float, N), input =>
      MapGlb(MapSeq(toGlobal(id)) o ReduceSeqUnroll(add, 0.0f)) o
        Slide(3,1) o
        Pad(1,1,clamp) $ input
  )

  val highLevel: Lambda1 = fun(
    ArrayType(Float, N), input =>
      Map(Reduce(add, 0.0f)) o
        Slide(3,1) o
        Pad(1,1,clamp) $ input
  )

  val dotprod: Lambda1 = fun(
    ArrayType(Float, N),
    input =>
      Reduce(add, 0.0f) o Map(abs) $ input
  )

  val gemvn = fun(
    ArrayType(ArrayType(Float, M), N),
    ArrayType(Float, M),
    ArrayType(Float, N),
    Float,
    Float,
    (matrix, vectorX, vectorY, alpha, beta) => {
      Map(fun(t =>
        Map(fun(x =>
          add(
            mult(x, alpha),
            mult(Get(t, 1), beta)
          )
        )) o
          Reduce(add, 0.0f) o
          Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(vectorX, Get(t, 0))
      )) $ Zip(matrix, vectorY)
    })
  val K = SizeVar("K")

  val mvasmm = fun(
    ArrayType(ArrayType(Float, K), N),
    ArrayType(ArrayType(Float, 1), K), // Column vector
    (matrix, vector) =>
      Map(fun(row =>
        Map( fun( col =>
          Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(row, col)
        )) o Transpose() $ vector
      )) $ matrix
  )

  val mapFun = UserFun("mapFun",
    Array("sX", "sY", "sZ", "Kx", "Ky", "Kz", "PhiMag"),
    """{
      |    #define PIx2 6.2831853071795864769252867665590058f
      |    float expArg = PIx2 * (Kx * sX + Ky * sY + Kz * sZ);
      |    Tuple2_float_float bla = { PhiMag * cos(expArg), PhiMag * sin(expArg) };
      |    return  bla;
      |}""".stripMargin,
    Seq(Float, Float, Float, Float, Float, Float, Float), TupleType(Float, Float))

  val reduceFun = UserFun("reduceFun",
    Array("x", "y"),
    """{
      | x._0 += y._0;
      | x._1 += y._1;
      | return x;
      }""".stripMargin,
    Seq(TupleType(Float, Float), TupleType(Float, Float)), TupleType(Float, Float))

  val xSize = SizeVar("X")
  val kSize = SizeVar("K")

  val myriqCompQ = fun(
    ArrayType(Float, xSize),
    ArrayType(Float, xSize),
    ArrayType(Float, xSize),
    ArrayType(TupleType(Float, Float, Float, Float), kSize),
    (x, y, z, kValues) =>
      Map(\(t =>
        Reduce(reduceFun, Value("{ 0.0f, 0.0f}", TupleType(Float, Float))) o
          Map(\(k => mapFun(t._0, t._1, t._2, k._0, k._1, k._2, k._3))) $ kValues
      )) $ Zip(x, y, z)
  )



  def main(args: Array[String]): Unit = {
    SDFPrinter("./", "dotprodjs", dotprod)
    SDFPrinter("./", "gemvnjs", gemvn)
    SDFPrinter("./", "mvasmmjs", mvasmm)
    SDFPrinter("./", "myriqCompQjs", myriqCompQ)
//    MyDotPrinter("./", "dotprod2", dotprod)
//    MyDotPrinter("./", "gemvn2", gemvn)
//    MyDotPrinter("./", "mvasmm2", mvasmm)
//    MyDotPrinter("./", "myriqCompQ2", myriqCompQ)
//    DotPrinter("./", "mvasmm", mvasmm)
//    DotPrinter("./", "myriqCompQ", myriqCompQ)
  }
}
