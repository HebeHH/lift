package ir.printer

import java.io.{File, PrintWriter, Writer}

import ir.{ArrayType, NoType, ScalarType, TupleType, UndefType, VectorType}

import scala.sys.process._
import ir.ast._
import rewriting.utils.NumberExpression

import lift.arithmetic._
import ir._
import ir.ast.Expr


/**
  * @author cdubach
  */
object TestPrinter {
  def apply(name: String, f: Lambda): Unit = {
    val home = System.getProperty("user.home")
    this.apply(home, name, f)
  }

  def apply(path: String, name: String, f: Lambda): Unit = {
    new TestPrinter(new PrintWriter(new File(s"$path/$name.txt"))).print(f)
  }

  def withNumbering(path: String, name: String, f: Lambda, compress : Boolean = true): Unit = {
    val numbering = NumberExpression.breadthFirst(f)
    new TestPrinter(new PrintWriter(new File(s"$path/$name.dot")), compress, false, false, numbering).print(f)
  }
}

class TestPrinter(w: Writer,
                 compressLambda : Boolean = true,
                 printAddressSpace : Boolean = false,
                 printRef : Boolean = false,
                 numbering : scala.collection.Map[Expr, Int] = scala.collection.Map()
                ) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Map[Any, Int] = collection.mutable.HashMap()
  lazy val nodesId : collection.mutable.Map[Any, Int] = collection.mutable.HashMap()
  lazy val counters : collection.mutable.Map[Param, Int]  = collection.mutable.HashMap()
  var node_ctr = 0

  def writeln(s: String): Unit = {
    w.write(s+"\n")
  }

  def getNodeId(n: Any): String = {
    if (!nodesId.contains(n)) {
      nodesId.put(n, node_ctr)
      node_ctr = node_ctr + 1
    }
    s"n${nodesId(n)}(${visited(n)})"
  }

  def print(node: IRNode): Unit = {
    node match {
      case l: Lambda => countParams(l.body)
      case fp: FPattern => countParams(fp.f.body)
      case e: Expr => countParams(e)
    }

    writeln("Nodes:")
    printNodes(node)

    visited.clear() // start counting again to associate the right nodes with the right edges
    writeln("\n\nEdges:")
    printEdges(node, "", "")

    w.flush()
    w.close()
  }

  def interrogateType(typ: Type, indents: Int) : String = {
    var info = ("    " * indents)
    typ match {
      case ScalarType(name, size) => info += s"Scalar $name (size = $size)"
      case VectorType(scalarT, len) => info += s"Vector (len = $len) {" + interrogateType(scalarT, indents + 1) +("    " * indents)+"}"
      case tup: TupleType=>
        info += s"Tuple ${tup.varList} {"
        for (t <- tup.elemsT) { info += interrogateType(t, indents + 1) + "-"}
        info += ("    " * indents) + "}"
      case arr: ArrayType =>
        info += s"Array (len = "
        //        TODO: UNDERSTAND LENGTH BETTER
        if (!arr.hasFixedAllocatedSize) info += arr.sizeIndex.toString else info += "N"
        info += ") {" + interrogateType(arr.elemT, indents + 1)
        info += ("    " * indents) + "}"
      case UndefType => info += "UndefType"
      case NoType => info += "NoType"
    }
    info
  }

  def interrogateParam(para: Param, indents: Int): String = {

    var info = ("    " * indents) + s"${getNodeId(para)}:${para.getClass.getSimpleName}:Param"

    para match {
      case Value(value, typ) => info += s"Value ($value, $typ)"
      case p: VectorParam => {
        info += s"Vector (${p.n.evalInt}) {"
        info += interrogateParam(p.p, indents + 1) + ("    " *  indents) + "}"
      }
      case _ => info += "Unknown"
    }
    info += "{"
    info += interrogateType(para.t, indents + 1) + ("    " * indents) + "}"
    info
  }

  def printNodes(node: IRNode): Unit = {

//    if (visited.contains(node) && !(node.isInstanceOf[FunDecl] || node.isInstanceOf[Value])){
//      writeln(node.getClass.getSimpleName + "is VISITED and NOT an instance of FunDecl or Value")
////      return
//    }
    visited.put(node, visited.getOrElse(node, 0)+1)

    val nodeId = getNodeId(node)

    node match {
      case fc: FunCall =>
        fc.f match {
          case fp: FPattern =>
          case p : Pattern =>
            writeNodeDef(fc)
            printNodes(p)
            fc.args.foreach(printNodes)
            return
          case _ =>
        }
        writeNodeDef(fc)
        fc.args.foreach(printNodes)
        printNodes(fc.f)

      case v: Value =>
        val number = if (numbering.contains(v)) numbering(v).toString else ""

        writeln(nodeId + ":" + node.getClass.getSimpleName + ":"+v.value+"" +
          (if (number != "")  number  else "") )

      case p: Param =>
        writeln(interrogateParam(p, 0))

      case l: Lambda =>
        l.body match {
          case fc: FunCall =>
            //            DEFINITELY KEEP THIS
            if (compressLambda)
              if (fc.args.length == l.params.length)
                if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2, 0) <= 2).forall(identity)) {
                  printNodes(fc.f)
                  return
                }

            l.params.foreach(p => printNodes(p))
            visited.put(fc, visited.getOrElse(fc, 0)+1)

            writeln(nodeId+":"+node.getClass.getSimpleName)
            writeNodeDef(fc)
            fc.args.foreach(printNodes)
            printNodes(fc.f)

            return
        }
        writeln(nodeId+":"+node.getClass.getSimpleName)
        l.params.foreach(p => printNodes(p))

        printNodes(l.body)

      case p: Pattern =>
        p match {
          case fp: FPattern =>
            writeln(nodeId+":"+node.getClass.getSimpleName)
            printNodes(fp.f)
          case Join() =>
            writeln(nodeId+":"+node.getClass.getSimpleName)
          case Split(chunkSize) =>
            writeln(nodeId+":"+node.getClass.getSimpleName+s" (chunksize = $chunkSize)")
          case t: Tuple =>
            writeln(nodeId+":"+t.n)
          case z: Zip =>
            writeln(nodeId+":"+node.getClass.getSimpleName)
          case u: Unzip =>
            writeln(nodeId+":Unzip")
          case  Transpose() =>
            writeln(nodeId+":"+node.getClass.getSimpleName)
          case Slide(size, step) =>
            writeln(nodeId+":"+node.getClass.getSimpleName + s" (size = $size, step = $step)")
          case Get(i) =>
            writeln(nodeId+":"+node.getClass.getSimpleName + s" ($i)")
          case  Filter() =>
            writeln(nodeId+":"+node.getClass.getSimpleName)
          case  _ =>
            writeln("It's an Unknown Pattern")
            writeln(nodeId+":"+node.getClass.getSimpleName)
        }

      case uf: UserFun =>
        val number = numbering.find{
          case (expr, _) => println(expr.getClass.getName); expr match {
            case FunCall(u : UserFun, _*) if uf == u => true
            case _ => false
          }
        }.map(x => "<BR/><i>" + x._2.toString + "*</i>").getOrElse("")
        val print = if (compressLambda) number else ""
        writeln(nodeId+":UserFun:"+uf.name+print)

      case  _ =>
        writeln(nodeId+":Unknown:"+node.getClass.getSimpleName)
    }
  }



  def printEdges(node : IRNode, parent: String, label: String, attr: String = "") : Unit = {
    if (!(visited.contains(node) && !(node.isInstanceOf[FunDecl] || node.isInstanceOf[Value]))) {
      visited.put(node, visited.getOrElse(node, 0) + 1)
    }

    val nodeId = getNodeId(node)


    node match {
      case fc: FunCall =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2))//, ",color=Red"))
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
      case l: Lambda =>
        if (compressLambda)
          l.body match {
            case fc: FunCall =>
              if (fc.args.length == l.params.length)
                if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters(p._2) <= 2).forall(identity)) {
                  printEdges(fc.f, parent, "f")
                  return
                }
          }
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))

        printEdges(l.body, nodeId, "body")

      case z: Zip =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
      case Unzip() =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case _ =>
        }
      case _ =>
        if (!parent.equals(""))
          writeln (s"$parent -> $nodeId: $label ($attr)")
    }
  }

  //  This writes either FunCalls or params
  def writeNodeDef(e: Expr): Unit = {
    val addrSpce = if (printAddressSpace) ":addrSpce("+e.addressSpace+")" else ""
    val number = if (numbering.contains(e)) "<BR/><i>" + numbering(e).toString + "</i>" else ""
    val ref = if (printRef) "@"+e.## else ""

    if (ref+addrSpce + number != "") println("Missed extra information: "+ ref+addrSpce + number)

    writeln(getNodeId(e) + ":" + e.getClass.getSimpleName)
  }





  def countParams(expr: Expr): Unit = {
    Expr.visit(expr,
      {
        case p: Param => counters.put(p, counters.getOrElse(p, 0) + 1)
        case _ =>
      }
      , post => {})
  }
}
