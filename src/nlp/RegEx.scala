/**
 * Created by Angeleyes on 7/27/2015.
 */
package nlp

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

class PostFix(input: String){
  val rpn = new Stack[Char]()
  val operator = new Stack[Char]()
  val operators: Set[Char] = Set('+', '*', '-', '(', ')', '/', '^')
  val precedence = Map( '^' -> 4,
                        '*' -> 3,
                        '/' -> 3,
                        '+' -> 2,
                        '-' -> 2,
                        '(' -> 0,
                        ')' -> 0)

  def in2post(input: String): String = {
    for(ch <- input) {
      if(ch.isLetterOrDigit) rpn.push(ch)
      if(operators.contains(ch)) {
        handleOperators(ch)
      }
    }
    while(!operator.isEmpty)
      rpn.push(operator.pop())
    rpn.mkString("").reverse
  }

  def handleOperators(input: Char): Unit = {
    input match {
      case '(' => operator.push(input)
      case ')' => {
        while(!operator.isEmpty && operator.top != '(')
          rpn.push(operator.pop())
        operator.pop()
        rpn.push('.')
      }
      case _ =>  {
        while(!operator.isEmpty && precedence(operator.top) >= precedence(input))
          rpn.push(operator.pop())
        operator.push(input)
      }
    }
  }
}

object PostFix{
  def main(args: Array[String]) {
    val obj = new PostFix("aa+bb")
    println(obj.in2post("a(bb)+a"))
  }
}
