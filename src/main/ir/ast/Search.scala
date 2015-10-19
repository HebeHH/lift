package ir.ast

import apart.arithmetic.{?, ArithExpr, Var}
import ir._

/**
 * Abstract class for search patterns.
 *
 * An object of the search pattern has to be instantiated with a given lambda `f`,
 * the comparator function used to search the array, therefore it is not possible to 
 * have a term like `Search()'
 *
 * f returns a value denoting the result of a comparison between the element of the array
 * passed to it, and a ``baked in'' element, which the search is looking for.
 *
 * The value returned must adhere to the following schema:
 *    f(e) <  0  iff e  > ix
 *    f(e) == 0  iff e == ix 
 *    f(e) >  0  iff e  < ix
 *
 *  This is slightly counterintuitive, however it allows for efficient comparisons (for example)
 *  between numeric types using a simple subtraction, for example:
 *
 *  int compare(int e, int ix) {
 *    return (ix-e)
 *  }
 *
 * @param f A lambda comparing a passed element to a baked in search element, similar to C's `bsearch' function
 */

abstract class AbstractSearch(val f: Lambda, 
                              val name: String) extends Pattern(arity = 2) 
                                                        with FPattern {
  assert(f.params.length == 1)
  var indexVar: Var = Var("ix")
  var searchFMem : Memory = UnallocatedMemory

  override def checkType(argType: Type, 
                         setType: Boolean): Type = {
    argType match {
      case TupleType(defaultT, ArrayType(elemT, _)) => 
        // check the default and element type match
        // TODO: Can this be done in the pattern match statement?
        if(defaultT != elemT) throw new TypeException(defaultT, elemT.toString)
        // check the direction function takes a single argument
        if(f.params.length != 1) throw new NumberOfArgumentsException
        // set the argument type to the array element type
        f.params(0).t = elemT
        // recursively check the body
        TypeChecker.check(f.body, setType)
        // ensure that the body function returns an integer
        // if(f.body.t != Int) throw new TypeException(f.body.t, "Int")
        // finally, return a single element array type
        ArrayType(elemT, 1)

      case _ => throw new TypeException(argType, "TupleType(a, ArrayType(a, _)")
    }
  }
}

case class BSearch(override val f: Lambda) extends AbstractSearch(f, "BSearch") with isGenerable{
  override def copy(f: Lambda): Pattern = BSearch(f)
}

object BSearch {
  def apply(f:Lambda, init: Expr) : Lambda1 = fun((x) => BSearch(f)(init, x))
}

case class LSearch(override val f: Lambda) extends AbstractSearch(f, "LSearch") with isGenerable{
  override def copy(f: Lambda): Pattern = LSearch(f)
}

object LSearch {
  def apply(f: Lambda, init: Expr) : Lambda1 = fun((x) => LSearch(f)(init, x))
}