/**
 * Created by Angeleyes on 7/27/2015.
 */
package nlp

import scala.collection.immutable.Set
import scala.collection.mutable.Stack

class PostFix(){
  val rpn = new Stack[Char]()
  val operator = new Stack[Char]()
  val operators: Set[Char] = Set('+', '*', '-', '(', ')', '/', '^')
  val unaryOperator: Set[Char] = Set('+', '*')
  val precedence = Map( '^' -> 4,
                        '*' -> 3,
                        '/' -> 3,
                        '+' -> 2,
                        '-' -> 2,
                        '(' -> 0,
                        ')' -> 0)

  def in2post(input: String): String = {
    for(ch <- input) {
      if(ch.isLetterOrDigit || ch == '.') rpn.push(ch)
      else if(operators.contains(ch) && !unaryOperator.contains(ch)) {
        handleOperators(ch)
      }
      else
        rpn.push(ch)
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
      }
      case _ =>  {
        while(!operator.isEmpty && precedence(operator.top) >= precedence(input))
          rpn.push(operator.pop())
        operator.push(input)
      }
    }
  }

  def appendConcatOperator(input: String): String = {
    var result: String = input.head.toString
    var i = 1
    val endConcat = if(result(0).isLetterOrDigit) true else false
    while(i < input.length-1){
      if(unaryOperator.contains(input(i)))
        result += input(i) + "."
      else if(!operators.contains(input(i)) && !unaryOperator.contains(input(i + 1)))
        result += input(i) + "."
      else if(unaryOperator.contains(input(i + 1)))
        result += input(i)
      else if(input(i) == '('){
        result += input(i) + appendConcatOperator(input.slice(i+1, input.indexOf(')'))) + ')'
        i = input.indexOf(')')
      }
      i += 1
    }
    result += input(input.length-1)
    if(endConcat) result += "."
    result
  }
}

object PostFix{
  def main(args: Array[String]) {
    val obj = new PostFix()
    val str = obj.appendConcatOperator("a(bb)+a")
    println(str)
    println(obj.in2post(str))
  }

}