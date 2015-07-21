package ir.ast

import apart.arithmetic.ArithExpr

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
  with isGenerable
