package cbackends.common.view

import core.generator.GenericAST
import core.generator.GenericAST.{ArithExpression, ExpressionT, IntConstant, StringConstant, VarRef}
import ir.{ArrayType, ArrayTypeWS, Size}
import ir.view._
import lift.arithmetic.ArithExpr

object ViewPrinter {


  def generateArrayAccess(the_view: View,
                          arrayAccessStack: List[ArithExpr],
                          tupleAccessStack: List[Int]): ExpressionT = {
    the_view match {

      case ViewAccess(i, iv, _) =>
        generateArrayAccess(iv, i::arrayAccessStack, tupleAccessStack)

      case ViewSplit(chunksize, iv, _) =>
        val chunkIdx :: elemIdx :: rest = arrayAccessStack
        val newIdx = chunkIdx * chunksize + elemIdx
        generateArrayAccess(iv, newIdx::rest, tupleAccessStack)

      case ViewConstant(value, _) =>
        GenericAST.RawCode(value.value)

      case ViewMem(memVar, ty) =>
        tupleAccessStack.isEmpty match {
          case true => assert(arrayAccessStack.size == 1);VarRef(memVar, arrayIndex = Some( ArithExpression(arrayAccessStack.head) ))
          case false => assert(false, "Not implement"); IntConstant(0)
        }

      case ViewTupleComponent(i, iv, _) =>
        val newTupleAccessStack = i :: tupleAccessStack
        generateArrayAccess(iv, arrayAccessStack, newTupleAccessStack)

      case ViewTuple(ivs, _) =>
        val i :: newTupleAccssStack = tupleAccessStack
        generateArrayAccess(ivs(i), arrayAccessStack, newTupleAccssStack)

      case ViewZip(iv, _) =>
        generateArrayAccess(iv, arrayAccessStack, tupleAccessStack)

      case ViewReorder(reindexFun, iv, _) =>
        val idx :: indices = arrayAccessStack
        generateArrayAccess(iv, reindexFun(idx) :: indices, tupleAccessStack)

      case ViewTranspose(iv, _) =>
        val top :: second :: rest = arrayAccessStack
        generateArrayAccess(iv, second::top::rest, tupleAccessStack)

      case ViewPad(iv, left, _, padFun, _) =>
        val idx :: indices = arrayAccessStack
        val currentIdx = idx - left
        val length = iv.t.asInstanceOf[ArrayType with Size].size
        val newIdx = if(ArithExpr.mightBeNegative(currentIdx) || ArithExpr.isSmaller(length -1, currentIdx.max).getOrElse(true))
          padFun(currentIdx, length)
        else
          currentIdx
        generateArrayAccess(iv, newIdx :: indices, tupleAccessStack)


      case ViewGeneratorUserFun(f, ArrayTypeWS(_, m)) =>
        assert(arrayAccessStack.length == 1)
        val i :: Nil = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, m).map(ArithExpression)
            /*
          List(i, m)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression) */
        )

      case View3DGeneratorUserFun(f, ArrayTypeWS(ArrayTypeWS(ArrayTypeWS(_, o), n), m)) =>
        val i :: j :: k :: _ = arrayAccessStack
        GenericAST.FunctionCall(
          f.name,
          List(i, j, k, m, n, o).map(ArithExpression)
          /*
          List(i, j, k, m, n, o)
            .map(ArithExpr.substitute(_, replacements))
            .map(ArithExpression)
            */
        )

      case ViewJoin(chunkSize, iv, _) =>
        val idx :: indices = arrayAccessStack
        val chunkIdx = idx / chunkSize
        val elemIdx = idx % chunkSize
        generateArrayAccess(iv, chunkIdx :: elemIdx :: indices, tupleAccessStack)
        //generateArrayAccess(iv,  elemIdx :: chunkIdx ::indices, tupleAccessStack)

      case ViewMapSeq(iv, itVar, _ ) =>
        val idx :: indices = arrayAccessStack
        generateArrayAccess(iv, indices, tupleAccessStack)


      case ViewMap(iv, itVar, _) =>
        val idx :: indices = arrayAccessStack
        val newV = iv.replaced(itVar, idx)
        generateArrayAccess(newV, indices, tupleAccessStack)

      case ViewSlide(iv, slide, _) =>
        val chunkIdx :: elemIdx :: indices = arrayAccessStack
        val newIdx = chunkIdx * slide.step + elemIdx
        generateArrayAccess(iv, newIdx :: indices, tupleAccessStack)

      case _ =>
        assert(false, "Pattern may not be implemented in view printer"); StringConstant("Unreachable!")

    }
  }

  def apply(view: View) : ExpressionT = {

    generateArrayAccess(view, List(), List())

  }

}
