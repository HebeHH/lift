package ir.printer

import ir.printer.AllPrograms._

object PrinterRunner {


  def main(args: Array[String]): Unit = {
    SDFPrinter("./", "dotprodjs", dot)
  }
}
