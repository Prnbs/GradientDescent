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

  def appendConcatRecursive(input: String, result: String): String = {
    var tempResult = result
    println(s"input = $input")
    println(s"input result $result")
    //terminating condition
    if(input.length == 0) tempResult
    else if(input.length == 1){
      tempResult += input + "."
    }
    else if(input(0) == '('){
      tempResult +=  "(" + input(1)
      tempResult = appendConcatRecursive(input.slice(2, input.indexOf(')')), tempResult) + ")"
      tempResult = appendConcatRecursive(input.slice(input.indexOf(')')+1, input.length), tempResult)
    }
    else if(unaryOperator.contains(input(0))){
      tempResult += input(0) + "."
      tempResult = appendConcatRecursive(input.slice(1, input.length), tempResult)
    }
    else if(!operators.contains(input(0)) && !unaryOperator.contains(input(1))){
      tempResult += input(0) + "."
      tempResult = appendConcatRecursive(input.slice(1, input.length), tempResult)
    }
    else if(unaryOperator.contains(input(1))){
      tempResult += input(0)
      tempResult = appendConcatRecursive(input.slice(1, input.length), tempResult)
    }
    println(s"result $tempResult")
    tempResult
  }

  def appendConcatOperator(input: String): String = {
    var result: String = input.head.toString
    var i = 1
    println(s"input $input")
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
    if(input(input.length-1) != ')')
      result += input(input.length-1)
    if(endConcat) result += "."
    println(s"result $result")
    result
  }
}

object PostFix{
  def main(args: Array[String]) {
    val obj = new PostFix()
    val input = "a(bb)+a(xxx)*"
    var str = input.head.toString
    str = obj.appendConcatRecursive(input.tail, str)
    println(str)
//    println(obj.in2post(str))
  }

}