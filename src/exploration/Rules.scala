package exploration


import scala.collection.Seq
import ir._
import opencl.ir._



object Rules {
   

  private def composedDerivations(l: Lambda, c: Constraints, level: Int): Seq[Lambda] = {
    l.callee match {
      case cf: CompFunDef =>
        val optionsList = cf.funs.map(f => derivsWithOneRule(f,c, level))
        Utils.listPossiblities(cf.funs, optionsList).map(funs => new Lambda(cf.params,(new CompFunDef(cf.params,funs: _*))(l.body.args:_*)))
    }
  }
   
   private def innerDerivations(fpat: Lambda, c: Constraints, level: Int): Seq[Lambda] = {
     fpat match {
       case fpat : FPattern =>
         val newCalleeList = derivsWithOneRule(fpat.f, c,level).map((f) => fpat.getClass.getConstructor(classOf[Lambda]).newInstance(f))
         newCalleeList.map(c => new Lambda(fpat.f.params, c(fpat.f.body.args:_*)))
     }
   }
    
    
  /*
   * Return a list of all possible derivations using only one rule at a given level
   */
  def derivsWithOneRule(f: Lambda, c: Constraints, level: Int): Seq[Lambda] =  {
      f.callee match {
        case cf: CompFunDef => composedDerivations(cf, c, level)
        case fp: FPattern if level==0 => outerDerivations(fp,c)
        case fp: FPattern if level>0 => innerDerivations(fp,c,level-1)
        case p: Pattern if level==0  => outerDerivations(p, c)
        case _ => List()
      }

  } 


  private def validOSplitRange(t: Type) = {
    t match {
      case ArrayType(_, len) => RangeMul(Cst(1), len, Cst(2))
      case _ => RangeUnkown // Error
    }
  }
 

  def outerDerivations(f: Lambda, c: Constraints): Seq[Lambda] = {

    val newCalleeList = f.callee match {

      case Map(inF) =>
        var result = List[FunDef]()
        
        // sequential
        if (!f.body.context.inSeq && (f.body.context.inMapGlb || f.body.context.inMapLcl))
        	result = result :+ MapSeq(inF)
        	
        // global, workgroup
        if (f.body.context.mapDepth == 0 && !f.body.context.inMapGlb && !f.body.context.inMapWrg) {
          result = result :+ MapGlb(inF)
          result = result :+ MapWrg(inF)
        }
        
        // local
        if (f.body.context.mapDepth == 1 && f.body.context.inMapWrg && !f.body.context.inMapGlb && !f.body.context.inMapLcl) {
          result = result :+ MapLcl(inF)
        }
        
        // split-join
        if (f.body.context.mapDepth+1 < c.maxMapDepth && !c.converge)
          result = result :+ (Join() o Map(Map(inF)) o Split(Var(validOSplitRange(f.body.inT))))
        
        result
      
      case Reduce(inF,init) =>
        var result = List[FunDef]()
        if (!c.converge)
        	result = result :+ (Reduce(inF,init) o PartRed(inF,init))
        	
        if (!f.body.context.inSeq && (f.body.context.inMapGlb || f.body.context.inMapLcl))
        	result = result :+ ReduceSeq(inF,init)

        if (!f.body.context.inMapGlb && !f.body.context.inMapLcl && !f.body.context.inMapWrg)
          result = result :+ ReduceHost(inF,init)
        	
        result
      
      case PartRed(inF,init) =>
        var result = List[FunDef]()
        result = result :+ Reduce(inF,init)
        if (f.body.context.mapDepth < c.maxMapDepth && !c.converge)
          result = result :+ (Join() o Map(PartRed(inF,init)) o Split(Var(validOSplitRange(f.body.inT))))
        result

      case _ => List[FunDef]() // all the terminals end up here
    }

    newCalleeList.map(fd => new Lambda(f.params, fd(f.body.args:_*)))
  }
  
  
  
  /*
   * Simplification and fusion rules.
   */
  /*def pairDeriv(f1: Fun, f2: Fun): List[Fun] = {
    f1 match {

      case Map(f,ctx1) => {
        f2 match {
          case Map(g,ctx2) => List(Map(new CompFun(f, g),ctx1)) // TODO: merge context ctx1 and ctx2
          case _ => List()
        }
      }

      case oSplit(_) => {
        f2 match {
          case oJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case iSplit(_) => {
        f2 match {
          case iJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case _ => List()      
    }
  }*/

}