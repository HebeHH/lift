package opencl.ir.pattern

import ir.ast._
import lift.arithmetic.{PosVar, Var}

case class MapAtomLcl(dim: Int, override val f: Lambda1, workVar: Var)
extends AbstractMap(f, "MapAtomLcl", PosVar("l_id")) {
  override def copy(f: Lambda): Pattern = MapAtomLcl(dim, f, PosVar("l_id"))
  var emitBarrier = true
}

object MapAtomLcl {
  def apply(f: Lambda1) = new MapAtomLcl(0, f, PosVar("work_idx")) // o is default

  def apply(dim: Int) = (f: Lambda1) => new MapAtomLcl(dim, f, PosVar("work_idx"))
}
