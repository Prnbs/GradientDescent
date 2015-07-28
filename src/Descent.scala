import Classes.MathHelperTypedefs._
import MathHelper.MatrixImpl._
import Trainees.TrainingDataGen

import scala.collection.parallel.immutable.ParVector

trait GradientDescentAPI{
  def fit(DataFrame: Matrix, size: Int, numFeatures: Int): Row
  def predict(Input: Matrix, theta: Row): Row
}


class GradientDescent extends GradientDescentAPI{

  def fit(DataFrame: Matrix, size: Int, numFeatures: Int): Row ={
    GradientDescent(DataFrame.tail, DataFrame.head, size, numFeatures)
  }

  def predict(Input: Matrix, theta: Row): Row ={
    for(row <- Input) yield dotProd(theta, row)
  }

  /*
    h(x) =  c + m1 x1 + m2 x2 + m3 x3 + ...
   */
  def HypothesisLinear(size: Int, theta: Row, X: Matrix): Row = {
    var Y: Row = ParVector()
    var i: Int = 0
    while(i < size)
    {
      Y = Y ++ ParVector(dotProd(theta, X(i)))
      i = i + 1
    }
    Y
  }

  def PartialDerivs(distance: ParVector[Double], X: Matrix, size: Int, num_features: Int): ParVector[Double] = {
    val derivatives: Array[Double]  = Array.fill(num_features)(0)
    var i:Int = 0
    while(i < num_features){
      derivatives(i) = X(i).zip(distance).map{t:(Double, Double) => t._1 * t._2}.sum/size
      i = i + 1
    }
    Vector(derivatives.toList: _*).par
  }

  def PartialDerivativesAndCost(size: Int,theta: Row, X: Matrix,
                                Y: Row, hypo: Row => Matrix => Row):(Double,Row)={
    //h(x) =  c + m1 x1 + m2 x2 + m3 x3 + ...
    val hypothesis: ParVector[Double] = hypo(theta)(X)
    //hypothesis(i) - y(i)
    val distance: ParVector[Double] = hypothesis.zip(Y).map{t:(Double, Double) => t._1 - t._2}
    // ((hypothesis(i) - y(i)) * (hypothesis(i) - y(i)) / 2 * m
    val costSquared = distance.map(scala.math.pow(_, 2))
    val cost: Double = costSquared.sum / (2 * size)
    // (hypothesis(i) - y(i)) * x(i)/m
    val partial_derive: ParVector[Double] = PartialDerivs(distance, X.T, size, theta.length)
    (cost, partial_derive)
  }

  def GradientDescent(i_var: Matrix, d_var: Row, size: Int, num_features: Int):Row ={
    var theta: Array[Double]  = Array.fill(num_features+1){0.0}
    var (cost_func_last: Double, num_iter, alpha, m) = (0.0, 0, 0.000003, size)
    var index: Int = 0
    var converged: Boolean = false
    val funName = (HypothesisLinear (_, _,_)).curried
    val total_iter = 20
    var currIter = 0
    while( currIter < total_iter){
      num_iter += 1
      //get cost function and partial derivatives
      val (cost_func, partial_derive) = PartialDerivativesAndCost(size, Vector(theta.toList: _*).par, i_var, d_var, funName(size))
      index = index + 1
      //theta = theta_old - alpha * partial_derivative
      val zippedList  = theta.zip(partial_derive).toList
      theta = zippedList.map{t: (Double, Double) => t._1 - (alpha * t._2)}.toArray
      theta(0) = zippedList.head._1 - (alpha * 400 * zippedList.head._2)
      if(cost_func > cost_func_last && cost_func_last != 0) {
        alpha = alpha /2
        println("Cost overshot, alpha was reduced")
      }
//      else if(cost_func_last > cost_func && (cost_func_last - cost_func) < 0.000005){
//        alpha = alpha * 10
//        println("Cost delta too low, increasing alpha")
//      }
      val res = theta.toList.mkString("[",",","]")
//      println(s"Theta: $res, Cost=$cost_func")
      if (cost_func < 0.0004)
        converged = true
      cost_func_last = cost_func
      currIter += 1
//      println(currIter)
    }
    println(cost_func_last)
    Vector(theta.toList: _*).par
  }

//  def ScaleFeatures(i_var: Matrix, num_features: Int): Matrix = {
//    val i_var_without_header: Matrix = i_var.tail
//    var result: Matrix = List(List.fill(i_var.head.length)(1.0))
//    var i: Int = 0
//    while(i < num_features){
//      val avg: Double = i_var_without_header(i).sum / i_var_without_header(i).length
//      val range: Double = i_var_without_header(i).max - i_var_without_header(i).min
//      if(range != 0)
//        result = result ::: List(i_var_without_header(i).map(x => (x - avg)/range))
//      else
//        result = result ::: List(i_var_without_header(i).map((_ - avg)))
//      i = i + 1
//    }
//    result
//  }
}

object Descent {
  def main(args: Array[String]) {
    val starter = new GradientDescent()
    val trainees = new TrainingDataGen()
    val size: Int = 300
    val num_features: Int = 6
    val DataFrame: Matrix = trainees.GenerateTrainingSet(size, num_features)
    println("------Y--------")
    println(DataFrame.head)
    println("------X--------")
    println(DataFrame.tail)
    val theta = starter.fit(DataFrame, size, num_features)
    println("----Calculated Theta----")
    println(theta)
    val predicted = starter.predict(DataFrame.tail, theta)
    println("---Predictions---")
    println(predicted)
  }
}