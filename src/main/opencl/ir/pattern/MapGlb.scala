package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.PosVar

case class MapGlb(dim: Int, override val f: Lambda1)
  extends AbstractMap(f, "MapGlbl", PosVar("gl_id")) {
  override def copy(f: Lambda): Pattern = MapGlb(dim, f)
}

object MapGlb {
  def apply(f: Lambda1) = new MapGlb(0, f) // 0 is default

  def apply(dim: Int) = (f: Lambda1) => new MapGlb(dim, f)
}
