package test

import scala.util.Random

object Exploration {
  
  val verbose = true

  def evalPerf(f: Fun, c: Constraints) : Double = {
    
    if (verbose) {
      println("------------------------------------------------------")
      println("Evaluating performance of "+f)
    }
    
    assert (f.inT != UndefType)      
    
    var perfs = List[Double]()
    val seen = scala.collection.mutable.Set[Fun]()
    for (i <- 1 to 10) {      
      val rndFun = search(f, f, new Constraints(c.maxMapDepth, true, true))

      if (!seen.contains(rndFun)) {
        seen += rndFun
        val perf = Random.nextDouble
        println(perf + " : " + rndFun)
        perfs = perfs :+ perf
      }
    }
    
    if (perfs.isEmpty)
      return Double.MaxValue  
      
    val sortedPerfs = perfs.sorted
    val median = sortedPerfs(sortedPerfs.length/2)
    
    median
    
    //Random.nextDouble
  }
  
  def choose(topF: Fun, oriF: Fun, choices: Seq[Fun], c: Constraints) : Fun = {    
        
    if (c.randomOnly == true)
    	return choices(Random.nextInt(choices.length))  
    
    val rndTerFixed : Constraints = new Constraints(c.maxMapDepth, true, true)
    rndTerFixed.fixedFuns = rndTerFixed.fixedFuns + oriF
    
    val perfMap = scala.collection.mutable.Map[Fun, List[Double]]()
    val seen = scala.collection.mutable.Set[Fun]()
    
    // generate a few random top level function with the oriF in place
    for (i <- 1 to 10) {
      
      val rndFun = search(topF, topF, rndTerFixed)
      
      if (!seen.contains(rndFun)) {
        seen += rndFun
        
        choices.map(choice => {          
          val f = Fun.replaceRef(rndFun, oriF, choice)
          Type.check(f, topF.inT)
          Context.updateContext(f, topF.context)
          val perf = evalPerf(f, c)
          perfMap.update(choice, perfMap.getOrElse(choice, List[Double]()) :+ perf)
        })
      }        
    }
    
    val medians = perfMap.map({case (key, vals) => {
      val sortedVals = vals.sorted
      val median = sortedVals(sortedVals.length/2)
      (key,median)
    }})
    
    medians.reduce((x,y) => if (x._2 < y._2) x else y)._1    
  }
      
  def search(topF: Fun, f: Fun, c: Constraints = new Constraints(3, false)) : Fun = {
      
    assert (f.inT    != UndefType)
    assert (topF.inT != UndefType)
    assert (f.context != null)
    assert (topF.context != null)    

    // first horizontally
    var bestH = f match {
      case cf: CompFun => {
        val newFuns = cf.funs.map(inF => search(topF, inF, c))
        if (newFuns.length == 1)
          return newFuns(0)
        else
          CompFun(newFuns: _*)
      }
      case p: Pattern => {
        val choices = Rules.outerDerivations(p, c)
        val bestPattern = if (choices.isEmpty) p else choose(topF, f, choices, c)
          
        // TODO: depending on what is returned, we have to search it again (PartRed -> Reduce -> ReduceSeq for instance)
        
        bestPattern match {
          case _ : CompFun => {
            // if we derived a composed function, look again for the best horizontally
            Type.check(bestPattern, p.inT)
            Context.updateContext(bestPattern, f.context)
            search(topF, bestPattern, c)            
          }
          case _ => bestPattern
          }
      }
      case _ => f
    } 
  
    Type.check(bestH, f.inT)
    Context.updateContext(bestH, f.context)
    
    // then vertically
    val bestV = bestH match {
      case fp :FPattern if c.canDerive(fp)=> fp.getClass().getConstructor(classOf[Fun]).newInstance(search(topF, fp.fun, c))      
      case _ => bestH        
    }
    
    bestV

  }
  
  /*
   * Returns a list of alternatives functions equivalent to f.
   * The return list does not include f. 
   */
  private def alternatives(f: Fun, inputType: Type, level: Int,
    constraints: Constraints = new Constraints(3, false)): Set[Fun] = {

    var c = constraints

    // setup the context
    //if (f.context == null)
      Context.updateContext(f, new Context())
    //else
      //Context.updateContext(f)

    // setup the types
    Type.check(f, inputType)

    Rules.derivsWithOneRule(f, c, level).toSet;
  }
  
  def bfs(f: Fun, inputType: Type, maxDepth: Int,      
      constraints: Constraints = new Constraints(3, false), level: Int = 0) : Set[Fun] = {

    var c = constraints
    
    if (level > maxDepth)
      return Set(f)

    var cnt = 0
    var alts = alternatives(f, inputType, level)
    var newAlts = alts
    while (newAlts.nonEmpty) {
      if (cnt > 2)
        c = new Constraints(c.maxMapDepth, true)
      newAlts = newAlts.map(r => alternatives(r, inputType, level, c)).flatten
      alts = alts ++ newAlts
      cnt = cnt+1
    }
    
    val results = alts.map(r => bfs(r, inputType, maxDepth, constraints, level+1)).flatten
    
    results + f
  }
  
}