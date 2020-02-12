package ir.printer


import ir.printer.AllPrograms._

object TestRunner {

  def main(args: Array[String]): Unit = {
//    DotPrinter("./", "gemvT",gemvT)
//    TestPrinter("./", "nbody_v4",nbody)
    TestPrinter("./printed/", "gemvN_v2", gemvN)
    TestPrinter("./printed/", "mmNN_v2", mmNN)
    TestPrinter("./printed/", "dot_v2", dot)
    TestPrinter("./printed/", "mmNT_v2", mmNT)
    TestPrinter("./printed/", "nbody_v2", nbody)
    TestPrinter("./printed/", "gesummvNN_v2", gesummvNN)
    TestPrinter("./printed/", "gemvT_v2", gemvT)
    TestPrinter("./printed/", "molecularDynamics_v2", molecularDynamics)
    TestPrinter("./printed/", "mriqComputeQ_v2", mriqComputeQ)
    TestPrinter("./printed/", "mandelbrot_v2", mandelbrot)
    TestPrinter("./printed/", "blackScholes_v2", blackScholes)
    TestPrinter("./printed/", "mriqPhiMag_v2", mriqPhiMag)
    TestPrinter("./printed/", "asum_v2", asum)
    TestPrinter("./printed/", "mv_v2", mv)
    TestPrinter("./printed/", "nearestNeighbour_v2", nearestNeighbour)
    TestPrinter("./printed/", "mmTT_v2", mmTT)
    TestPrinter("./printed/", "scal_v2", scal)
    TestPrinter("./printed/", "mvAsMM_v2", mvAsMM)
    TestPrinter("./printed/", "kmeans_v2", kmeans)
    TestPrinter("./printed/", "gesummvTT_v2", gesummvTT)
    TestPrinter("./printed/", "mmTN_v2", mmTN)
  }
}
