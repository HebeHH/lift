package ir.printer


import ir.printer.AllPrograms._

object TestRunner {

  def main(args: Array[String]): Unit = {
    SDFPrinter("./", "dotprodv1", dot)
  }
}
