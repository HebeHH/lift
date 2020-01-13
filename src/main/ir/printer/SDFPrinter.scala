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
object SDFPrinter {
  def apply(name: String, f: Lambda): Unit = {
    val home = System.getProperty("user.home")
    this.apply(home, name, f)
  }

  def apply(path: String, name: String, f: Lambda): Unit = {
    new SDFPrinter(new PrintWriter(new File(s"$path/$name.txt"))).print(f)
  }

  def withNumbering(path: String, name: String, f: Lambda, compress : Boolean = true): Unit = {
    val numbering = NumberExpression.breadthFirst(f)
    new SDFPrinter(new PrintWriter(new File(s"$path/$name.dot")), compress, false, false, numbering).print(f)
  }
}

class SDFPrinter(w: Writer,
                 compressLambda : Boolean = true,
                 printAddressSpace : Boolean = false,
                 printRef : Boolean = false,
                 numbering : scala.collection.Map[Expr, Int] = scala.collection.Map()
                ) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Map[Any, Int] = collection.mutable.HashMap()
  lazy val counters : collection.mutable.Map[Param, Int]  = collection.mutable.HashMap()
  var node_ctr = 0
  val nodesId : collection.mutable.Map[Int, Int] = collection.mutable.HashMap()

  var json_str = ""
  def writeln(s: String): Unit = {
    json_str += s + "\n"
//    w.write(s+"\n")
  }

  def getNodeId(n: Any): String = {
    val tmp = Math.abs(n.hashCode())
    if (!nodesId.contains(tmp)) {
      nodesId.put(tmp, node_ctr)
      node_ctr = node_ctr + 1
    }
    s"n${nodesId(tmp)}(${visited(n)})"
  }

  def print(node: IRNode): Unit = {
    node match {
      case l: Lambda => countParams(l.body)
      case fp: FPattern => countParams(fp.f.body)
      case e: Expr => countParams(e)
    }

    val st = "{\n"+"\""+s"Nodes"+"\""+s" : {"
    writeln(st)
    printNodes(node)
    writeln("},\n")
    visited.clear() // start counting again to associate the right nodes with the right edges

    writeln("\n"+"\""+s"Edges"+"\""+s" : [")
    printEdges(node, "", "")
    writeln("]\n}")

    json_str = json_str.replaceAll(",[ \t\r\n]+}", "}").replaceAll(",[ \t\r\n]+]", "]")
    w.write(json_str)

    w.flush()
    w.close()
  }

  def interrogateType(typ: Type, indents: Int) : String = {
    var info = ("    " * indents)
    typ match {
      case ScalarType(name, size) => info += s"{ "+"\""+s"Type"+"\" : \""+s"scalar"+"\", \""+s"Kind"+"\" : \""+name+"\",\"Size\" : \""+s"$size"+"\" },"
      case VectorType(scalarT, len) => info += s"{ "+"\""+s"Type"+"\" : \""+s"vector"+"\", \""+s"Kind"+"\""+s" : \n" + interrogateType(scalarT, indents + 1) +"\n "+"\""+s"Length"+"\""+s" :"+"\""+s"$len"+"\""+s" }," // s"Vector (len = $len) {\n" + interrogateType(scalarT, indents + 1) +"\n"+("    " * indents)+"},"
      case tup: TupleType=>
        info += s"{"+"\""+s"Type"+"\" : \""+s"tuple"+"\", \""+s"Kinds"+"\""+s" : [\n"
        for (t <- tup.elemsT) { info += interrogateType(t, indents + 1)}
        info += ("    " * indents) + "]},"
      case arr: ArrayType =>
        info += s"{"+"\""+s"Type"+"\" : \""+s"array"+"\", \""+s"Kind"+"\""+s" : \n${interrogateType(arr.elemT, indents + 1)}\n "+"\""+s"Length"+"\" : \""+s""
        //        TODO: UNDERSTAND LENGTH BETTER
        if (!arr.hasFixedAllocatedSize) info += arr.sizeIndex.toString else info += "N"
        info += ""+"\""+s"},"
      case UndefType => info += s"{ "+"\""+s"Type"+"\" : \""+s"undef"+"\""+s" },"
      case NoType => info += s"{ "+"\""+s"Type"+"\" : \""+s"notype"+"\""+s" },"
    }
    info + "\n"
  }

  def interrogateParam(para: Param, indents: Int): String = {
    var x = para.getClass.getSimpleName

    var info = ("    " * indents) +"\""+s"${getNodeId(para)}"+"\" : { "+"\""+s"Class"+"\" : \""+s"${para.getClass.getSimpleName}"+"\", \"Para\" :"

    para match {
      case Value(value, typ) => info += "\""+s"Value"+"\""+", "+"\""+s"value"+"\" : \""+s"$value"+"\", \""+s"Type"+"\" : \""+s"$typ"+"\""+s","
      case p: VectorParam =>
        info += "\""+s"Vector"+"\""+s" , "+"\""+s"eint"+"\" : \""+s"${p.n.evalInt},"+"\""+s" , "+"\""+s"Type"+"\""+s" : \n"
        info += interrogateParam(p.p, indents + 1) + ("    " *  indents)
      case _ => info += ""+"\""+s"Unknown"+"\""+s","
    }
    info += "\"Kind\" : \n"
    info += interrogateType(para.t, indents + 1) + "},"
    info
  }

  def printNodes(node: IRNode): Unit = {

    if (visited.contains(node) && !(node.isInstanceOf[FunDecl] || node.isInstanceOf[Value]))
      return
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

        writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\", \"Value\" : \""+v.value+"\"" +
          (if (number != "")  ", \"Number"+"\" : \""+s"$number"+"\""  else "") + "}, " )

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

//            writeln(nodeId+" "+node.getClass.getSimpleName)
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\""+s"},")
            writeNodeDef(fc)
            fc.args.foreach(printNodes)
            printNodes(fc.f)

            return
        }
//        writeln(nodeId+": "+node.getClass.getSimpleName)
        writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\""+s"},")
        l.params.foreach(p => printNodes(p))

        printNodes(l.body)

      case p: Pattern =>
        p match {
          case fp: FPattern =>
//            writeln(nodeId+": "+node.getClass.getSimpleName)
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\""+s"},")
            printNodes(fp.f)
          case Split(chunkSize) =>
//            writeln(nodeId+": "+node.getClass.getSimpleName+s" (chunksize = $chunkSize)")
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\", \""+s"Chunksize"+"\" : \""+s"$chunkSize"+"\"},")
          case Slide(size, step) =>
//            writeln(nodeId+": "+node.getClass.getSimpleName + s" (size = $size, step = $step)")
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\", \""+s"size"+"\" : \""+s"$size"+"\", \""+s"step"+"\""+s" :"+"\""+s"$step"+"\"},")
          case Get(i) =>
//            writeln(nodeId+": "+node.getClass.getSimpleName + s" ($i)")
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\", \""+s"s"+"\" : \""+s"$i"+"\"},")
          case t: Tuple =>
            writeln(nodeId+": "+t.n)
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${t.n},"+"\""+s"},")
          case z: Zip =>
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\""+s"},")
          case u: Unzip =>
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \"Unzip"+"\"},")
          case  _ =>
            writeln("\""+s"$nodeId"+"\" : { "+"\""+s"Class"+"\" : \""+s"${node.getClass.getSimpleName}"+"\", \""+s"Warning"+"\" : \"True\"},")
        }

      case uf: UserFun =>
        val number = numbering.find{
          case (expr, _) => println(expr.getClass.getName); expr match {
            case FunCall(u : UserFun, _*) if uf == u => true
            case _ => false
          }
        }.map(x => "<BR/><i>" + x._2.toString + "*</i>").getOrElse("")
        val print = if (compressLambda) number else ""
//        writeln(nodeId+": UserFun : "+uf.name+print)
        writeln("\""+nodeId+"\" : { \"Class\" : \"UserFun\", \"Kind\" : \""+uf.name+"\"},")

      case  _ =>
        writeln("\""+nodeId+"\" :  { \"Class\" : \"Unknown\" , \"Kind\" :\""+node.getClass.getSimpleName+"\"},")
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
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2))//, ",color=Red"))
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
        if (!parent.equals(""))
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
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
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))

        printEdges(l.body, nodeId, "body")

      case z: Zip =>
        if (!parent.equals(""))
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
      case Unzip() =>
        if (!parent.equals(""))
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case _ =>
        }
      case _ =>
        if (!parent.equals(""))
          writeln(s"{"+"\""+s"Source"+"\" : \""+s"$parent"+"\", \""+s"Sink"+"\" : \""+s"$nodeId"+"\", \""+s"Label"+"\" : \""+s"$label"+"\", \""+s"Attr"+"\""+s": "+"\""+s"$attr"+"\""+s"},")
    }
  }

//  This writes Funcalls
  def writeNodeDef(e: Expr): Unit = {
    val addrSpce = if (printAddressSpace) ":addrSpce("+e.addressSpace+")" else ""
    val number = if (numbering.contains(e)) "<BR/><i>" + numbering(e).toString + "</i>" else ""
    val ref = if (printRef) "@"+e.## else ""

    if (ref+addrSpce + number != "") println("Missed extra information: "+ ref+addrSpce + number)

    writeln("\"" + getNodeId(e) + "\": {\"Class\":\"" + e.getClass.getSimpleName + "\"},")
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
