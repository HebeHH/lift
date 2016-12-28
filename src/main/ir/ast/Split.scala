package ir.ast

import lift.arithmetic.ArithExpr
import ir.interpreter.Interpreter._
import ir.{ArrayType, Type, TypeException, UndefType}

/**
 * Split pattern.
 * Code for this pattern can be generated.
 *
 * The split pattern has the following high-level semantics:
 *   `Split(n)( [x,,1,,, ..., x,,m,,] ) =
 *     [ [x,,1,,, ..., x,,n,,], ..., [x,,m-n+1,,, ..., x,,m,,] ]`
 *
 * The split pattern has the following type:
 *   `Split(n) : [a],,n x j,, -> [ [a],,n,, ],,j,,`
 *
 * We know the following algorithmic rewrite rules for the split pattern
 * (so far):
 *  - `Join() o Split(chunkSize) | Split(chunkSize) o Join() => id`
 *
 * @param chunkSize The size of chunks the input array should be split in.
 *                  The size of the input array must be a multiple of
 *                  `chunkSize`.
 */
case class Split(chunkSize: ArithExpr) extends Pattern(arity = 1)
  with isGenerable {

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayType(t, n) =>
        ArrayType(ArrayType(t, chunkSize), n /^ chunkSize)

      case _ => throw new TypeException(argType, "ArrayType")
    }
  }


  override def eval(valueMap: ValueMap, args: Any*): Vector[Vector[_]] = {
    assert(args.length == arity)

    args.head match {
      case v: Vector[_] => v.grouped(chunkSize.eval).toVector
    }
  }
}
