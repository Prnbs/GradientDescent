/**
 * Created by Angeleyes on 7/27/2015.
 */
package nlp

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

class PostFix() {
  val rpn = new Stack[Char]()
  val operator = new Stack[Char]()
  val operators: Set[Char] = Set('+', '*', '-', '(', ')', '/', '^')
  val unaryOperator: Set[Char] = Set('+', '*')
  val precedence = Map('^' -> 4,
    '*' -> 3,
    '/' -> 3,
    '+' -> 2,
    '-' -> 2,
    '(' -> 0,
    ')' -> 0)

  def in2post(input: String): String = {
    for (ch <- input) {
      if (ch.isLetterOrDigit || ch == '.') rpn.push(ch)
      else if (operators.contains(ch) && !unaryOperator.contains(ch)) {
        handleOperators(ch)
      }
      else
        rpn.push(ch)
    }
    while (!operator.isEmpty)
      rpn.push(operator.pop())
    rpn.mkString("").reverse
  }

  def handleOperators(input: Char): Unit = {
    input match {
      case '(' => operator.push(input)
      case ')' => {
        while (!operator.isEmpty && operator.top != '(')
          rpn.push(operator.pop())
        operator.pop()
      }
      case _ => {
        while (!operator.isEmpty && precedence(operator.top) >= precedence(input))
          rpn.push(operator.pop())
        operator.push(input)
      }
    }
  }

  def appendConcat(input: String, result: String): String = {
    if(input.length == 0) return result
    var tempResult = result + input.head
    //terminating condition
    if (input.length == 1) return tempResult + "."

    if (input(0) == '(') {
      tempResult += input(1)
      tempResult = appendConcat(input.slice(2, input.indexOf(')')), tempResult) + ")"
      tempResult = appendConcat(input.slice(input.indexOf(')') + 1, input.length), tempResult)
    }
    else{
      if(!unaryOperator.contains(input(1))){
        tempResult += "."
      }
      tempResult = appendConcat(input.slice(1, input.length), tempResult)
    }
    tempResult
  }
}

object PostFix{
  def main(args: Array[String]) {
    val obj = new PostFix()
    val input = "ab(dd)*(de)+"
    var str = input.head.toString
    str = obj.appendConcat(input.tail, str)
    println(str)
    println(obj.in2post(str))
  }

}