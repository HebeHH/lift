package exploration

import java.io.{File, FileWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.typesafe.scalalogging.Logger
import exploration.detection.{DetectCommunicationBetweenThreads, DetectReuseAcrossThreads, DetectReuseWithinThread, ImplementReuse}
import ir._
import ir.ast._
import opencl.ir.pattern._
import org.clapper.argot.ArgotConverters._
import org.clapper.argot._
import rewriting._
import rewriting.macrorules.MacroRules
import rewriting.rules._
import rewriting.utils.{DumpToFile, NumberExpression}
import rewriting.utils.Utils.getHash

import scala.io.Source

object MemoryMappingRewrite {

  private val logger = Logger(this.getClass)

  private val parser = new ArgotParser("MemoryMappingRewrite")

  protected[exploration] val defaultVectorize = true
  protected[exploration] val defaultVectorWidth = 4
  protected[exploration] val defaultSequential = false
  protected[exploration] val defaultLoadBalancing = false
  protected[exploration] val defaultUnrollReduce = false
  protected[exploration] val defaultGlobal0 = false
  protected[exploration] val defaultGlobal01 = false
  protected[exploration] val defaultGlobal10 = false
  protected[exploration] val defaultGlobal012 = false
  protected[exploration] val defaultGlobal210 = false
  protected[exploration] val defaultGroup0 = false
  protected[exploration] val defaultGroup01 = false
  protected[exploration] val defaultGroup10 = false

  //local-memory rules
  protected[exploration] val defaultAddIdForCurrentValueInReduce = true
  protected[exploration] val defaultAddIdMapLcl = true
  protected[exploration] val defaultAddIdMapWrg = true
  protected[exploration] val defaultAddIdAfterReduce = true

  parser.flag[Boolean](List("h", "help"),
    "Show this message.") {
    (sValue, _) =>
      parser.usage()
      sValue
  }

  private val input = parser.parameter[String]("input",
    "Input file containing the lambda to use for rewriting",
    optional = false) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage("Input file \"" + s + "\" does not exist")
      s
  }

  private val settingsFile = parser.option[String](List("f", "file"), "name",
    "The settings file to use."
    ) {
    (s, _) =>
      val file = new File(s)
      if (!file.exists)
        parser.usage(s"Settings file $file doesn't exist.")
      s
  }

  protected[exploration] val vectorize : FlagOption[Boolean] = parser.flag[Boolean](List("vectorize"),
    s"Apply vectorization (default: $defaultVectorize)")

  protected[exploration] val vectorWidth : SingleValueOption[Int] = parser.option[Int](List("vectorWidth", "vw"), "vector width",
    s"The vector width to use for vectorising rewrites (default: $defaultVectorWidth)")

  private[exploration] val sequential : FlagOption[Boolean] = parser.flag[Boolean](List("s", "seq", "sequential"),
    s"Don't execute in parallel (default: $defaultSequential)")

  private[exploration] val loadBalancing : FlagOption[Boolean] = parser.flag[Boolean](List("l", "lb", "loadBalancing"),
    s"Enable load balancing using MapAtomLocal and MapAtomWrg (default: $defaultLoadBalancing)")

  private[exploration] val unrollReduce : FlagOption[Boolean] = parser.flag[Boolean](List("u", "ur", "unrollReduce"),
    s"Additionally generate expressions also using ReduceSeqUnroll (default: $defaultUnrollReduce)")

  private[exploration] val global0 : FlagOption[Boolean] = parser.flag[Boolean](List("gl0", "global0"),
    s"Mapping: MapGlb(0)( MapSeq(...) ) (default: $defaultGlobal0)")

  private[exploration] val global01 : FlagOption[Boolean] = parser.flag[Boolean](List("gl01", "global01"),
    s"Mapping: MapGlb(0)(MapGlb(1)( MapSeq(...) )) (default: $defaultGlobal01)")

  private[exploration] val global10 : FlagOption[Boolean] = parser.flag[Boolean](List("gl10", "global10"),
    s"Mapping: MapGlb(1)(MapGlb(0)( MapSeq(...) )) (default: $defaultGlobal10)")

  private[exploration] val global012 : FlagOption[Boolean] = parser.flag[Boolean](List("gl012", "global012"),
    s"Mapping: MapGlb(0)(MapGlb(1)(MapGlb(2)( MapSeq(...) ))) (default: $defaultGlobal012)")

  private[exploration] val global210 : FlagOption[Boolean] = parser.flag[Boolean](List("gl210", "global210"),
    s"Mapping: MapGlb(2)(MapGlb(1)(MapGlb(0)( MapSeq(...) ))) (default: $defaultGlobal210)")

  private[exploration] val group0 : FlagOption[Boolean] = parser.flag[Boolean](List("gr0", "group0"),
    s"Mapping: MapWrg(0)(MapLcl(0)( MapSeq(...) )) (default: $defaultGroup0)")

  private[exploration] val group01 : FlagOption[Boolean] = parser.flag[Boolean](List("gr01", "group01"),
    s"Mapping: MapWrg(0)(MapWrg(1)(MapLcl(0)(MapLcl(1)( MapSeq(...) )))) (default: $defaultGroup01)")

  private[exploration] val group10 : FlagOption[Boolean] = parser.flag[Boolean](List("gr10", "group10"),
    s"Mapping: MapWrg(1)(MapWrg(0)(MapLcl(1)(MapLcl(0)( MapSeq(...) )))) (default: $defaultGroup10)")

  private[exploration] val addIdForCurrentValueInReduce : FlagOption[Boolean] = parser.flag[Boolean](List("addIdForCurrentValueInReduce"),
    s"Enable local memory rule: addIdForCurrentValueInReduce (default: $defaultAddIdForCurrentValueInReduce)")

  private[exploration] val addIdMapLcl : FlagOption[Boolean] = parser.flag[Boolean](List("addIdMapLcl"),
    s"Enable local memory rule: addIdMapLcl (default: $defaultAddIdMapLcl)")

  private[exploration] val addIdMapWrg : FlagOption[Boolean] = parser.flag[Boolean](List("addIdMapWrg"),
    s"Enable local memory rule: addIdMapWrg (default: $defaultAddIdMapWrg)")

  private[exploration] val addIdAfterReduce : FlagOption[Boolean] = parser.flag[Boolean](List("addIdAfterReduce"),
    s"Enable local memory rule: addIdAfterReduce (default: $defaultAddIdAfterReduce)")

  private var settings = Settings()

  def main(args: Array[String]): Unit = {

    try {

      parser.parse(args)
      settings = ParseSettings(settingsFile.value)
      val config = settings.memoryMappingRewriteSettings

      val enabledMappings =
        EnabledMappings(
          config.global0,
          config.global01,
          config.global10,
          config.global012,
          config.global210,
          config.group0,
          config.group01,
          config.group10
        )

      if (!enabledMappings.isOneEnabled) scala.sys.error("No mappings enabled")

      logger.info(s"Settings:\n$settings")
      logger.info(s"Arguments: ${args.mkString(" ")}")
      logger.info("Defaults:")
      logger.info(s"\tVector width: $defaultVectorWidth")
      logger.info(s"\tMappings: $enabledMappings")

      val topFolder = input.value.get

      val all_files = Source.fromFile(s"$topFolder/index").getLines().toList

      val counter = new AtomicInteger()

      val allFilesPar = if (config.sequential) all_files else all_files.par

      allFilesPar.foreach(filename => {

        val count = counter.incrementAndGet()
        val hash = filename.split("/").last
        val numLambdas = all_files.size

        print(s"\rLowering : $count / $numLambdas")

        try {

          val lambda = ParameterRewrite.readLambdaFromFile(filename)
          val lowered = lowerLambda(lambda, enabledMappings, config.unrollReduce, topFolder)

          lowered.foreach(dumpToFile(topFolder, hash, _))

        } catch {
          case t: Throwable =>
            logger.warn(s"Reading $hash from file", t)
        }
      })

      println()

    } catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def lowerLambda(lambda: Lambda, enabledMappings: EnabledMappings, unroll: Boolean = false, hash: String = ""): Seq[Lambda] = {

    try {

      val loweredExpressions =
        Lower.mapCombinations(lambda, enabledMappings) ++ moveReduceAndMap(lambda, enabledMappings)

      val loadBalancedExpressions = loweredExpressions.flatMap(
        mapAddressSpaces(_, hash).flatMap(addressMapped => {

          val loadBalanced = mapLoadBalancing(addressMapped, hash)
          filterIllegals(loadBalanced)

        })
      )

      if(unroll) {
        val unrolledReduces = loadBalancedExpressions.filter(
          lambda => lambda.body.contains(
            { case FunCall(ReduceSeq(_), _*) => })).map(lambdaWithReduceSeq => {

          val rewrites = Rewrite.listAllPossibleRewrites(lambdaWithReduceSeq, OpenCLRules.reduceSeqUnroll)
          rewrites.foldLeft(lambdaWithReduceSeq)((expr, pair) =>
            Rewrite.applyRuleAt(expr, pair._2, pair._1))
        })

        unrolledReduces ++ loadBalancedExpressions
      } else
        loadBalancedExpressions

    } catch {
      case t: Throwable =>
        logger.warn(s"Lowering $hash failed.", t)
        Seq()
    }
  }

  private def moveReduceAndMap(lambda: Lambda, enabledMappings: EnabledMappings): List[Lambda] = {
    if (enabledMappings.group0) {
      val reduceDeeper = Lower.pushReduceDeeper(lambda)

      if (getHash(lambda) != getHash(reduceDeeper))
        return Lower.mapCombinations(reduceDeeper)
    }

    List()
  }

  def filterIllegals(lambdaSeq: Seq[Lambda], hash: String = ""): Seq[Lambda] = {

    lambdaSeq.filter(lambda =>
      try {
        // Sanity checks.
        if (lambda.body.contains({ case FunCall(Id(), _) => }))
          throw new RuntimeException("Illegal Id")

        if (lambda.body.contains({ case FunCall(Map(l), _) if l.body.isConcrete => }))
          throw new RuntimeException(s"Illegal un-lowered Map")

        true
      } catch {
        case t: Throwable =>
          //noinspection SideEffectsInMonadicTransformation
          logger.warn(s"Illegal lambda in $hash failed, ${t.toString}")
          false
      })
  }

  def dumpToFile(topFolder: String, hash: String, expr: Lambda): Unit = {
    // Dump to file
    val str = DumpToFile.dumpLambdaToMethod(expr)
    val sha256 = DumpToFile.Sha256Hash(str)
    val folder = s"${topFolder}Lower/$hash/" + sha256.charAt(0) + "/" + sha256.charAt(1)

    if (DumpToFile.dumpToFile(str, sha256, folder)) {
      // Add to index if it was unique
      synchronized {
        val idxFile = new FileWriter(s"${topFolder}Lower/$hash/index", true)
        idxFile.write(folder + "/" + sha256 + "\n")
        idxFile.close()
      }
    }
  }

  /**
    * Returns all combinations with at most one load balancing rule applied
    *
    * @param lambda The lambda where to apply load balancing.
    */
  def  mapLoadBalancing(lambda: Lambda, hash: String): Seq[Lambda] = {
    if (!settings.memoryMappingRewriteSettings.loadBalancing)
      return Seq(lambda)

    try {
      val locations =
        Rewrite.listAllPossibleRewritesForRules(lambda, Seq(OpenCLRules.mapAtomWrg, OpenCLRules.mapAtomLcl))

      val withLoadBalancing = locations.map(pair => Rewrite.applyRuleAt(lambda, pair._2, pair._1))
      withLoadBalancing :+ lambda
    } catch {
      case _: Throwable =>
        logger.warn(s"Load balancing for $hash failed.")
        Seq()
    }
  }

  def mapAddressSpaces(lambda: Lambda, hash: String): Seq[Lambda] = {
    try {

      val compositionToPrivate =
        Rewrite.applyRuleUntilCannot(lambda, MacroRules.userFunCompositionToPrivate)

      val f = compositionToPrivate
      val strategicLocationsMarked =
        MemoryMappingRewrite.addIdsForPrivate(MemoryMappingRewrite.addIdsForLocal(f))

      val localMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
        DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
        distinct.
        filter(x => OpenCLRules.localMemory.isDefinedAt(x._1))

      val lambdas = ImplementReuse(strategicLocationsMarked, localMemCandidates, OpenCLRules.localMemory)

      val cleanedLambdas = lambdas.map(MemoryMappingRewrite.cleanup) :+ f

      val communication = cleanedLambdas.flatMap(lambda => {
        import DetectCommunicationBetweenThreads._
        val communicationHere = getCommunicationExpr(lambda)

        val implemented = communicationHere.flatMap(location => {
          try {
            Seq(implementCopyOnCommunication(lambda, location, OpenCLRules.localMemory))
          } catch {
            case _: Throwable =>
              Seq()
          }
        })

        val implementedGlobal = communicationHere.flatMap(location => {
          try {
            Seq(implementCopyOnCommunication(lambda, location, OpenCLRules.globalMemory))
          } catch {
            case _: Throwable =>
              Seq()
          }
        })

        implemented ++ implementedGlobal
      }) ++ cleanedLambdas

      val cleanedWithPrivate = communication.flatMap(lowered => {
        val strategicLocationsMarked =
          MemoryMappingRewrite.addIdsForPrivate(lowered)

        val privateMemCandidates = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
          DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
          distinct.
          filter(x => OpenCLRules.privateMemory.isDefinedAt(x._1))

        val lambdas = ImplementReuse(strategicLocationsMarked, privateMemCandidates, OpenCLRules.privateMemory)

        lambdas.map(MemoryMappingRewrite.cleanup)
      }) ++ communication

      val tupleFusion = cleanedWithPrivate.map(applyLoopFusionToTuple)

      implementIds(tupleFusion)

    } catch {
      case t: Throwable =>
        logger.warn(s"Address space mapping for $hash failed.", t)
        Seq()
    }
  }

  def implementIds(lambdas: Seq[Lambda]): Seq[Lambda] = {

    var list = List[Lambda]()

    lambdas.foreach(x => {
      try {
        list = lowerMapInIds(x) :: list
      } catch {
        case t: Throwable =>
         logger.warn("Id map lowering failure. Continuing...", t)
      }
    })

    list
  }

  private[exploration] def cleanup(lambda: Lambda) = {

    val cleanupRules = Seq(
      SimplificationRules.removeEmptyMap,
      SimplificationRules.lambdaInlineParam,
      SimplificationRules.dropId)

    Rewrite.applyRulesUntilCannot(lambda, cleanupRules)
  }

  private[exploration] def addIdsForLocal(lambda: Lambda): Lambda = {
    val config = settings.localMemoryRulesSettings

    val enabledRules = scala.collection.Map(
      CopyRules.addIdForCurrentValueInReduce -> config.addIdForCurrentValueInReduce,
      CopyRules.addIdBeforeMapLcl -> config.addIdMapLcl,
      CopyRules.addIdForMapWrgParam -> config.addIdMapWrg).flatMap( x => {
        val rule = x._1
        val enabled = x._2
        if(enabled) Some(rule)
        else None
    }).toSeq

    // TODO: Why?
    assert(enabledRules.nonEmpty)

    val firstIds = Rewrite.applyRulesUntilCannot(lambda, enabledRules)

    if (config.addIdAfterReduce) {

      val reduceSeqs = Expr.visitLeftToRight(List[Expr]())(firstIds.body, (e, s) =>
        e match {
          case call@FunCall(_: ReduceSeq, _*) => call :: s
          case _ => s
        }).filterNot(e => firstIds.body.contains({ case FunCall(toGlobal(Lambda(_, c)), _*) if c eq e => }))

      reduceSeqs.foldRight(firstIds)((e, l) => Rewrite.applyRuleAt(l, e, CopyRules.addIdAfterReduce))
    } else
      firstIds

  }

  private[exploration] def addIdsForPrivate(lambda: Lambda) = {
    val idsAdded = Rewrite.applyRuleUntilCannot(lambda, CopyRules.addIdForCurrentValueInReduce)

    UpdateContext(idsAdded)

    val (mapSeq, _) = Expr.visitLeftToRight((List[Expr](), false))(idsAdded.body, (expr, pair) => {
      expr match {
        case FunCall(toLocal(_), _) =>
          (pair._1, true)
        case call@FunCall(MapSeq(l), _)
          if !pair._2
            && (call.context.inMapLcl.reduce(_ || _) || call.context.inMapGlb.reduce(_ || _))
            && !l.body.contains({ case FunCall(uf: UserFun, _) if uf.name.startsWith("id") => })
            && CopyRules.addIdBeforeMapSeq.isDefinedAt(call)
        =>
          (call :: pair._1, false)
        case _ =>
          pair
      }
    })

    val idsAddedToMapSeq =
      mapSeq.foldLeft(idsAdded)((l, x) => Rewrite.applyRuleAt(l, x, CopyRules.addIdBeforeMapSeq))

    idsAddedToMapSeq
  }

  private def lowerMapInIds(lambda: Lambda): Lambda = {

    val depthMap = NumberExpression.byDepth(lambda).filterKeys({
      case FunCall(map: ir.ast.AbstractMap, _) if map.f.body.isConcrete => true
      case _ => false
    })

    val byDepth = depthMap.groupBy(_._2).mapValues(_.keys.toList).filter(pair => pair._2.exists({
      case FunCall(Map(_), _) => true
      case _ => false
    }))

    val depth = byDepth.map(pair => {
      val level = pair._1
      val expressions = pair._2

      val (nonLowered, lowered) = expressions.partition({
        case FunCall(_: Map, _) => true
        case _ => false
      })

      (level, lowered, nonLowered)
    })

    val idMap = NumberExpression.breadthFirst(lambda)

    var lowered = lambda

    depth.foreach(tuple => {
      val toLower = tuple._3
      val lowerToType = tuple._2

      val rule = lowerToType match {
        case FunCall(_: MapSeq, _) :: _ => OpenCLRules.mapSeq
        case FunCall(MapLcl(dim, _), _) :: _ => OpenCLRules.mapLcl(dim)
        case _ => OpenCLRules.mapSeq // Fall back to seq
      }

      toLower.foreach(expr => {
        val id = idMap(expr)
        lowered = Rewrite.applyRuleAtId(lowered, id, rule)

      })
    })

    lowered
  }

  private[exploration] def applyLoopFusionToTuple(lambda: Lambda): Lambda =
    Rewrite.applyRuleUntilCannot(lambda, FusionRules.tupleMap)


  private[exploration] def getCombinations[T](localIdList: Seq[T], max: Int): Seq[Seq[T]] = {
    if (localIdList.nonEmpty)
      (0 to max).map(localIdList.combinations(_).toSeq).reduce(_ ++ _).toArray.toSeq
    else
      Seq()
  }

  private[exploration] def getCombinations[T](localIdList: Seq[T]): Seq[Seq[T]] =
    getCombinations(localIdList, localIdList.length)

}

