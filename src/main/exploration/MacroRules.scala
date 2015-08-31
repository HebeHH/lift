package exploration

import apart.arithmetic.{?, ArithExpr}
import ir.{ArrayType, TypeChecker}
import ir.ast._

object MacroRules {
  // transpose both sides + id
  val transposeMapMapTranspose =
    Rule("Transpose() o Map(Map(Transpose())) => " +
      "Map(Map(Transpose())) o Transpose()", {
      case FunCall(Transpose(),
      FunCall(Map(Lambda(p1,
      FunCall(Map(Lambda(p2,
      FunCall(Transpose(), a2))
      ), a1))
      ), arg))
        if (p1.head eq a1) && (p2.head eq a2)
      =>
        Map(Map(Transpose())) o Transpose() $ arg
    })

  val mapFissionAtPosition: Int => Rule = position => Rule("", {
    case funCall @ FunCall(Map(Lambda(_, FunCall(_, _*))), _) => mapFissionAtPosition(position, funCall)
  })

  def mapFissionAtPosition(position: Int, expr: Expr): Expr = {
    var nextFission = expr
    var fissioned = expr
    var currentPos = position

    while (currentPos >= 0) {

      val replacement = Rules.mapFission.rewrite(nextFission)
      fissioned = Expr.replace(fissioned, nextFission, replacement)

      nextFission = replacement match {
        case FunCall(_: AbstractPartRed, _, arg) => arg
        case FunCall(_, arg) => arg
      }

      currentPos -= 1
    }

    currentPos = position
    var fused = fissioned

    while (currentPos > 0) {

      fused = Rules.mapFusion.rewrite(fused)

      currentPos -= 1
    }

    fused
  }

  val tileMapMap: (Int, Int) => Rule = (x, y) =>
    Rule("Tile a computation in the form Map(fun(y => Map(f) $ y )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if lambdaParam.head eq arg
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, Rules.transposeBothSides)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, Rules.splitJoin(y))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, Rules.transposeBothSides)
        e3
    })

  val tileOutput: Int => Rule = x =>
    Rule("Tile the output of a computation in the form " +
      "Map(fun(y => Map(f) $ z )) $ x", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), _)
        if !(lambdaParam.head eq arg)
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 2, Rules.mapMapInterchange)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 3, Rules.splitJoin(x))
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 5, Rules.mapMapInterchange)
        e3
    })

  val finishTilingInput: Int => Rule = x =>
    Rule("Map(x => Map(y => Map() $ Get(x, ...) Get$(x, ...) $ Zip(...)", {
      case funCall @
        FunCall(Map(Lambda(p,
        FunCall(Map(Lambda(_,
        FunCall(Map(_), FunCall(Get(_), a2)))),
        FunCall(Get(_), a1)))),
        FunCall(Zip(_), _*))
        if (p.head eq a1) && (p.head eq a2)
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 0, Rules.splitJoin(x))
        val e1 = Rewrite.applyRuleAtId(e0, 1, Rules.splitZip)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, Rules.mapMapTransposeZipOutside)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 4, Rules.mapMapTransposeZipOutside)
        e3
    })

  val tileInputAndOutput: (Int, Int) => Rule = (x, y) =>
    Rule("Tile the input and output of a computation in the form " +
      "Map(fun(x => Map(fun(y => Reduce(g) o Map(f) $ Zip(x, y) )) $ ... )) $ ...", {
      case funCall @ FunCall(Map(Lambda(lambdaParam1,
      FunCall(Map(Lambda(lambdaParam2,
      FunCall(Reduce(_), _, FunCall(Map(Lambda(_, FunCall(_: UserFun, _*))),
      FunCall(Zip(_), zipArgs@_*)))
      )), arg)
      )), _)
        if !(lambdaParam1.head eq arg)
          && zipArgs.contains(lambdaParam1.head)
          && zipArgs.contains(lambdaParam2.head)
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileOutput(x))

        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 7, Rules.mapFission)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 14, Rules.mapMapTransposeZipInside)
        val e4 = Rewrite.depthFirstApplyRuleAtId(e3, 6, mapFissionAtPosition(1))
        val e5 = Rewrite.depthFirstApplyRuleAtId(e4, 16, Rules.mapMapTransposeZipInside)

        val e6 = Rewrite.depthFirstApplyRuleAtId(e5, 17, finishTilingInput(y))

        e6
    })

  val moveTransposeInsideTiling =
    Rule("Map(Split(n) o Transpose()) o Split(m) o Transpose() => " +
      "Transpose() o Map(Transpose()) o Split(n) o Map(Split(m))", {
      case funCall @
        FunCall(Map(Lambda(_, FunCall(Split(_), FunCall(Transpose(), _)))),
        FunCall(Split(_), FunCall(Transpose(), _)))
      =>
        val e0 = Rewrite.depthFirstApplyRuleAtId(funCall, 4, Rules.splitTranspose)
        val e1 = Rewrite.depthFirstApplyRuleAtId(e0, 0, Rules.mapFusion)
        val e2 = Rewrite.depthFirstApplyRuleAtId(e1, 2, Rules.transposeTransposeId)
        val e3 = Rewrite.depthFirstApplyRuleAtId(e2, 0, Rules.mapSplitTranspose)
        e3
    })

  val finishRectangularTiles =
    Rule("", {
      case funCall@FunCall(Map(Lambda(_,
      FunCall(TransposeW(), FunCall(Join(),
      FunCall(Map(_), FunCall(Split(_), FunCall(Transpose(), _)))))
      )), FunCall(Split(_), _:Param))
        if Rules.mapFissionWithZipInside.rewrite.isDefinedAt(funCall)
          && Rules.mapFissionWithZipInside.rewrite.isDefinedAt(
          Rewrite.getExprForId(funCall, 5, NumberExpression.breadthFirst(funCall)))
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 5, Rules.mapFissionWithZipInside)
        val e4 = Rewrite.applyRuleAtId(e1, 6, moveTransposeInsideTiling)
        val e5 = Rewrite.applyRuleAtId(e4, 0, Rules.mapFissionWithZipInside)
        val e6 = Rewrite.applyRuleAtId(e5, 1, Rules.mapFission)
        val e7 = Rewrite.applyRuleAtId(e6, 2, Rules.mapTransposeSplit)
        val e8 = Rewrite.applyRuleAtId(e7, 1, Rules.mapSplitTranspose)

        e8
    })

  val tileTranspose: (Int, Int) => Rule = (x, y) =>
    Rule("Map(Map(f)) o Transpose() => tiled", {
      case funCall @ FunCall(Map(Lambda(lambdaParam, FunCall(Map(_), arg))), FunCall(Transpose(), _))
        if lambdaParam.head eq arg
      =>
        val e1 = Rewrite.applyRuleAtId(funCall, 0, tileMapMap(x, y))
        val e2 = Rewrite.applyRuleAtId(e1, 1, mapFissionAtPosition(2))
        val e3 = Rewrite.applyRuleAtId(e2, 2, moveTransposeInsideTiling)
        e3
    })

  val transposeBothSidesWithSplit =
    Rule("Transpose() o Map( ... o ... o Split(n) ) o Transpose() => " +
      "Map( ... ) o Map(Transpose) o Split(n)", {
      case funCall @
        FunCall(Transpose(),
        FunCall(Map(Lambda(p, FunCall(_, FunCall(_, FunCall(Split(_), a))))),
        FunCall(Transpose(), _)))
        if p.head eq a
      =>
        val e0 = Rewrite.applyRuleAtId(funCall, 1, mapFissionAtPosition(1))
        val e1 = Rewrite.applyRuleAtId(e0, 2, Rules.mapSplitTranspose)
        val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.transposeBothSides)
        val e3 = Rewrite.applyRuleAtId(e2, 0, Rules.transposeTransposeId)
        val e4 = Rewrite.applyRuleAtId(e3, 1, Rules.transposeTransposeId)
        e4
    })

  val reduceMapFusion = Rule("Reduce o Map => ReduceSeq(fused)", {
    case funCall @ FunCall(Reduce(_), _, mapCall@FunCall(Map(_), _))
      if Rules.mapSeq.isDefinedAt(mapCall)
    =>
      val e0 = Rewrite.applyRuleAtId(funCall, 1, Rules.mapSeq)
      val e1 = Rewrite.applyRuleAtId(e0, 0, Rules.reduceSeq)
      val e2 = Rewrite.applyRuleAtId(e1, 1, Rules.reduceSeqMapSeqFusion)
      e2
  })

  // Eliminate a matching split-join pair. If there are maps between them, apply
  // the split-join and splitJoinId rules, to move the pair closer to each other
  val splitJoinId: Rule = Rule("Split(n) ... Join() => Map(...)", {
    case call@FunCall(Split(n), arg)
      if Utils.visitFunCallChainWithState((false, true))(arg, (expr, state) => {
        // State = (applicable, stillPossible)
        expr match {
          case FunCall(Map(_), _) => state
          case FunCall(Join(), a)
            if (a.t match {
              case ArrayType(ArrayType(_, m), _) => n == m
              case _ => false
              })
          =>
            if (state._2)
              (true, state._1)
            else
              state
          case _ => (state._1, false)
        }
      })._1
    =>

      if (Rules.splitJoinId.isDefinedAt(call)) {
        Rules.splitJoinId.rewrite(call)
      } else {
        val splitJoined = Rules.splitJoin(n).rewrite.apply(arg)
        val newCall = Expr.replace(call, arg, splitJoined)

        TypeChecker.check(newCall)

        val splitJoinEliminated = Rules.splitJoinId.rewrite.apply(newCall)

        val newSplit = Utils.getExprForPatternInCallChain(splitJoinEliminated,
          {case FunCall(Split(_), _) => }).get

        Expr.replace(splitJoinEliminated, newSplit, splitJoinId.rewrite.apply(newSplit))
      }
  })

  private val reducePattern: PartialFunction[Expr, Unit] =
    { case FunCall(_: AbstractPartRed, _, _) => }

  // Map(Reduce) interchange, fissions as appropriate, automatically chooses the rule to apply
  val moveReduceOutOneLevel = Rule("Map( ... Reduce(f) ...) => Map(...) o Reduce( Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), arg)
      if innerCall.contains(reducePattern)
    =>

      var rule = Rules.mapReduceInterchange

      val reduceArg = Utils.getExprForPatternInCallChain(innerCall, reducePattern).get.asInstanceOf[FunCall].args(1)
      val reduceId = Utils.getIndexForPatternInCallChain(innerCall, reducePattern)

      var offset = 1

      val pattern: PartialFunction[Expr, Unit] =
        { case FunCall(Reduce(_), _,
               FunCall(Join(),
               FunCall(Map(Lambda(_, FunCall(PartRed(_), _, _))), _))) => }

      val patternId = Utils.getIndexForPatternInCallChain(innerCall, pattern)

      val zipPattern: PartialFunction[Expr, Unit] = { case FunCall(Zip(_), _*) => }

      val finalArg = Utils.getFinalArg(innerCall)
      val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
        { case e if e eq finalArg => })

      var fissioned: Expr = call

      if (patternId != -1) {
        rule = Rules.mapReducePartialReduce
        offset = 3
      }

      if (zipPattern.isDefinedAt(arg) && reduceArg.isAbstract) {
        rule = Rules.mapReduceInterchangeWithZipOutside

        var numFissions = finalArgId - reduceId - 2

        while (numFissions > 0) {
          fissioned = Rules.mapFissionWithZipOutside.rewrite(fissioned)
          numFissions -= 1
        }

      } else if (finalArgId > reduceId + offset) {
          fissioned = mapFissionAtPosition(reduceId + offset - 1).rewrite(fissioned)
      }

      if (reduceId > 0)
        fissioned = mapFissionAtPosition(reduceId - 1).rewrite(fissioned)

      val mapReduce = Utils.getExprForPatternInCallChain(fissioned,
      { case e if rule.isDefinedAt(e) => }).get

      TypeChecker(fissioned)
      Expr.replace(fissioned, mapReduce, rule.rewrite(mapReduce))
  })

  private val mapPattern: PartialFunction[Expr, Unit] =
    { case FunCall(map: Map, _) if map.f.body.isConcrete => }

  // Interchange the outer map with the first inner concrete map, fissions appropriately,
  // automatically chooses the rule to apply
  val mapMapInterchange = Rule("Map( ... Map(f) ...) => Map(...) o Map(Map(f)) o Map(...)", {
    case call@FunCall(Map(Lambda(_, innerCall: FunCall)), _)
      if innerCall.contains(mapPattern)
    =>
      val mapId = Utils.getIndexForPatternInCallChain(innerCall, mapPattern)

      var fissioned: Expr = call

      if (mapId > 0)
        fissioned = mapFissionAtPosition(mapId - 1).rewrite(fissioned)

      val mapCall = Utils.getExprForPatternInCallChain(fissioned, mapPattern).get

      val zipInside = Rules.mapMapTransposeZipInside.rewrite
      val zipOutside = Rules.mapMapTransposeZipOutside.rewrite
      val interchange = Rules.mapMapInterchange.rewrite
      val transpose = Rules.transposeBothSides.rewrite

      if (zipInside.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, zipInside(mapCall))
      else if (zipOutside.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, zipOutside(mapCall))
      else if (interchange.isDefinedAt(mapCall))
        Expr.replace(fissioned, mapCall, interchange(mapCall))
      else {

        val finalArg = Utils.getFinalArg(innerCall)
        val finalArgId = Utils.getIndexForPatternInCallChain(innerCall,
        { case e if e eq finalArg => })

        if (finalArgId > mapId + 1) {
          val id = if (mapId > 0) 1 else 0
          fissioned = Rewrite.applyRuleAtId(fissioned, id, mapFissionAtPosition(0))
        }

        val newMapCall = Utils.getExprForPatternInCallChain(fissioned, mapPattern).get

        TypeChecker.check(fissioned)
        Expr.replace(fissioned, newMapCall, transpose(newMapCall))
      }
  })

  val apply1DRegisterBlocking: Rule = apply1DRegisterBlocking(?)

  def apply1DRegisterBlocking(factor: ArithExpr): Rule = Rule("1D register blocking", {
    case call@FunCall(Map(Lambda(lambdaArg, innerCall)), _)
      if getCallForBlocking(innerCall, lambdaArg).isDefined
    =>

      // Split-join on outermost map
      val split = Rules.splitJoin(factor).rewrite.apply(call)

      // Interchange on the newly created dimension
      val interchanged = Rewrite.depthFirstApplyRuleAtId(split, 2, mapMapInterchange)

      // Interchange again on every map/reduce in the innermost dimension

      val map1 = Utils.getExprForPatternInCallChain(interchanged, mapPattern).get
      val map2 = Utils.getExprForPatternInCallChain(getMapBody(map1), mapPattern).get
      val map3 = Utils.getExprForPatternInCallChain(getMapBody(map2), mapPattern).get

      var nextToInterchange = map3
      var innerInterchanged = interchanged

      val reduceRule = moveReduceOutOneLevel.rewrite
      val mapRule = mapMapInterchange.rewrite

      val funCallPattern: PartialFunction[Expr, Unit] = { case FunCall(_, _) => }

      while (funCallPattern.isDefinedAt(nextToInterchange)) {

        TypeChecker(interchanged)

        if (reduceRule.isDefinedAt(nextToInterchange)) {
          val replacement = reduceRule(nextToInterchange)
          innerInterchanged = Expr.replace(innerInterchanged, nextToInterchange, replacement)

          replacement match {
            case FunCall(_, FunCall(_, _, next)) =>
              nextToInterchange = next
            case _ =>
          }

        } else if (mapRule.isDefinedAt(nextToInterchange)) {
          val replacement = mapRule(nextToInterchange)

          innerInterchanged = Expr.replace(innerInterchanged, nextToInterchange, replacement)

          replacement match {
            case FunCall(_, FunCall(_, next)) =>
              nextToInterchange = next
            case _ =>
          }
        } else {
          nextToInterchange match {
            case FunCall(_, next) =>
              nextToInterchange = next
            case _ =>
          }
        }

      }

      innerInterchanged
  })

  val apply2DRegisterBlocking: Rule = apply2DRegisterBlocking(?, ?)

  def apply2DRegisterBlocking(factorX: ArithExpr, factorY:ArithExpr): Rule = Rule("", {
    case call@FunCall(_, _) =>

      // ReorderBothSides of inner map
      // fission out the reorder
      // split-join on the outer map
      // interchange on the new dim
      // split-join on the inner map
      // interchange on the new dim
      // interchange inside stuff twice.

      call
  })

  private def getMapBody(expr: Expr) = {
    expr match {
      case FunCall(Map(Lambda(_, body)), _) => body
    }
  }

  private def getCallForBlocking(innerCall: Expr, lambdaArg: Array[Param]): Option[Expr] = {
    Utils.getExprForPatternInCallChain(innerCall, {
      case FunCall(Map(f), arg)
        if f.body.isConcrete
       =>
    })
  }
}
