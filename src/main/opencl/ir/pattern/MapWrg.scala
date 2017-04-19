package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.PosVar

case class MapWrg(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapWrg", PosVar("wg_id")) {
  override def copy(f: Lambda): Pattern = MapWrg(dim, f)
}


object MapWrg {
  def apply(f: Lambda1) = new MapWrg(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapWrg(dim, f)
}
