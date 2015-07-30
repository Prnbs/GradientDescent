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

abstract class State

case class Consume(val c: Char, val out: State) extends State

case class Split(val out1: State, val out2: State) extends State

case class Match() extends State

case class Placeholder(var pointingTo: State) extends State

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

  def ast2Nfa(ast: RegexExpr, andThen: State): State = {
    ast match {
      case Literal(c) => new Consume(c, andThen)
      case Concat(left, right) =>
        ast2Nfa(left, ast2Nfa(right, andThen))
      case Repeat(c) => {
        val placeholder = new Placeholder(null)
        val split = new Split(
          ast2Nfa(c, placeholder),
          andThen
        )
        placeholder.pointingTo = split
        placeholder
      }
      case Plus(c) => ast2Nfa(Concat(c, Repeat(c)), andThen)
    }
  }

    def evaluateNFA(currState: State, stringToMatch: String): Boolean = {
      currState match {
        case Consume(c, andThen) => {
          if(stringToMatch.isEmpty) return true
          if(stringToMatch.head != c) return false
          evaluateNFA(andThen, stringToMatch.tail)
        }
        case Placeholder(pointingTo) => {
          evaluateNFA(pointingTo, stringToMatch)
        }
        case Split(out1, out2) => {
          evaluateNFA(out1, stringToMatch)
          evaluateNFA(out2, stringToMatch)
        }
        case Match() => {
          return true
        }
      }
    }

  def main(args: Array[String]) {
    val obj = new PostFix()
    val input = "a(b)*"
    var str = input.head.toString
    str = obj.appendConcat(input.tail, str)
    val postFix = obj.in2post(str)
    println(s"Postfixed $postFix")
    val expr = postFix2AST(postFix)
    val state = ast2Nfa(expr, Match())
    val result: Boolean = evaluateNFA(state, "aaaaaabbbbbbbbbbbbbbbbbbb")
    println(result)

  }
}
