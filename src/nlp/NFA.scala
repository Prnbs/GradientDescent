package nlp

import scala.collection.mutable.Stack

/**
 * Created by psinha4 on 7/29/2015.
 */

abstract class RegexExpr
// ., a, b
case class Literal(c: Char) extends RegexExpr
// a|b
case class Or(expr1: RegexExpr, expr2: RegexExpr) extends RegexExpr
// ab -> Concat(a,b); abc -> Concat(a, Concat(b, c))
case class Concat(first: RegexExpr, second: RegexExpr) extends RegexExpr
// a*
case class Repeat(expr: RegexExpr) extends RegexExpr
// a+
case class Plus(expr: RegexExpr) extends RegexExpr

object NFA {

  def postFix2AST(postFixInput: String): RegexExpr = {
    val AST: Stack[RegexExpr] = Stack()
    postFixInput.foreach {
      case '*' =>
                  val expr = AST.pop()
                  AST.push(new Repeat(expr))
      case '+' =>
                  val expr = AST.pop()
                  AST.push(new Plus(expr))
      case '.' =>
                  val right = AST.pop()
                  val left = AST.pop()
                  AST.push(new Concat(left, right))
      case default => AST.push(new Literal(default))
    }
    require(AST.size == 1, "Multiple elements present in AST")
    AST.pop()
  }

  def main(args: Array[String]) {
    val obj = new PostFix()
    val input = "ab(dd)*"
    var str = input.head.toString
    str = obj.appendConcat(input.tail, str)
    val postFix = obj.in2post(str)
    println(s"Postfixed $postFix")
    var expr = postFix2AST(postFix)
    println("Done")

  }
}
