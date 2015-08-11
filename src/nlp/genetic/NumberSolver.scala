package nlp.genetic

import scala.collection.mutable.{ListBuffer, Stack}

/**
 * Created by psinha4 on 8/7/2015.
 */
class NumberSolver {

  var answer = new Stack[Float]()
  var operator = new Stack[Char]()
  var weights = new ListBuffer[Float]()
  var chromosomes = new ListBuffer[String]()

  def encode(character: Char): String ={
    character match{
      case '0' => "0000"
      case '1' => "0001"
      case '2' => "0010"
      case '3' => "0011"
      case '4' => "0100"
      case '5' => "0101"
      case '6' => "0110"
      case '7' => "0111"
      case '8' => "1000"
      case '9' => "1001"
      case '+' => "1010"
      case '-' => "1011"
      case '*' => "1100"
      case '/' => "1101"
      case _ => {
        println("Got " + character)
        "0"
      }
    }
  }

  def decode(pattern: String): Char ={
    pattern match{
      case "0000" => '0'
      case "0001" => '1'
      case "0010" => '2'
      case "0011" => '3'
      case "0100" => '4'
      case "0101" => '5'
      case "0110" => '6'
      case "0111" => '7'
      case "1000" => '8'
      case "1001" => '9'
      case "1010" => '+'
      case "1011" => '-'
      case "1100" => '*'
      case "1101" => '/'
      case _ => '|'
    }
  }

  def createChromosome(input: String): String = {
    var result = ""
    for(i <- 0 to input.length-1){
      result += encode(input(i))
    }
    result
  }

  def computeResult(input: String): Float = {
    var data = input
    val defective = -9999999.0f
    var decodedString = ""
    //decode the bit field to a string of numbers and operators
    while(data.length != 0){
      decodedString += decode(data.take(4))
      data = data.substring(4, data.length)
    }
    //push numbers to answer stack and operators to operator stack
    for(i <- 0 to decodedString.length-1){
      val item = decodedString(i) - '0'
      if(item >=0 && item <= 9){
        answer.push(item)
      }
      else{
        decodedString(i) match{
          case '+' => operator.push('+')
          case '-' => operator.push('-')
          case '*' => operator.push('*')
          case '/' => operator.push('/')
          case _ => {
//                      println("Diseased gene")
                      return defective
                    }
        }
      }
      if(answer.size == 2 && operator.size == 1){
        val b = answer.pop()
        val a = answer.pop()
        operator.pop match {
          case '+' => answer.push(a+b)
          case '-' => answer.push(a-b)
          case '*' => answer.push(a*b)
          case '/' => answer.push(a/b)
        }
      }
      //result is diseased is Number Operator Number Operator pattern is not followed
      else if(answer.size > 2 || operator.size > 1){
//        println("Diseased gene")
        return defective
      }
    }
    answer.pop
  }

  def populateRandomPopulation(size: Int):Unit = {
    for(i <- 1 to size){
      val input = getRandomString()
      val result = computeResult(createChromosome(input))
      if(result != Float.NegativeInfinity && result != Float.PositiveInfinity && !(result equals( Float.NaN)))
      {
        chromosomes += input
        weights  += result
//        println(result)
      }
    }
  }

  /**
   * Create a random chromosome
   * @return
   */
  def getRandomString():String = {
    val setOfNumbers = List('0', '1', '2', '3', '4', '5', '6', '7','8','9')
    val setOfOperators = List('+', '-',  '*',  '/')
    var result = ""
    for(i <- 1 to ((Math.random()*10).toInt%8)+2){
      result += setOfNumbers((Math.random()*10).toInt)
      result += setOfOperators((Math.random()*10).toInt%4)
    }
    result.take(result.length-1)
  }

  /**
   * Taken from wikipedia : https://en.wikipedia.org/wiki/Fitness_proportionate_selection#Java_-_linear_O.28n.29_version
   *
   * @return the index in weights Array which the roulette wheel stops at
   */
  def rouletteSelect(): Int = {
    val sumOfWeight = weights.sum
    //get a random value
    var randValue = Math.random() * sumOfWeight
    println(randValue)
    for(i <- 0 to weights.length-1){
      randValue -= weights(i)
      if(randValue <= 0) return i
    }
    return weights.length-1
  }

  def removeSeletedChromosome(index: Int): Unit = {
    chromosomes.remove(index)
    weights.remove(index)
  }

  def selectTwoMembers():Unit = {
    val firstMember = rouletteSelect()
    removeSeletedChromosome(firstMember)
    val secondMember = rouletteSelect()
    println(s"firstMember = $firstMember, secondMember = $secondMember ")

  }


}

object NumberSolver{
  def main(args: Array[String]) {
    val solver = new NumberSolver()
    solver.populateRandomPopulation(10000)
//    val t = solver.rouletteSelect()
//    println(solver.chromosomes(t))
//    println(solver.weights(t))
    solver.selectTwoMembers()
  }
}