import Classes.MathHelper._
import MathHelper.MatrixImpl._

class Descent {

  def GenerateIndependentFeatures(size: Int, num_features: Int): Matrix = {
    var i: Int = 1
    var result: Matrix = List(List.fill(size)(1))
    while(i <= num_features)
    {
      result = result ::: List(List.tabulate(size)(n=> (n+n*i+i).toDouble))
      i = i + 1
    }
    result
  }

  def GenerateDependentFeatures(size:Int, X: Matrix, Theta: Row): Row = {
    var Y: Row = List()
    var i: Int = 0
    while(i < size)
    {
      Y = Y ::: List(dotProd(Theta, X(i)))
      i = i + 1
    }
    Y
  }

  def GenerateTrainingSet(size:Int, num_features: Int):(Row, Matrix) = {
    val Xmat: Matrix = GenerateIndependentFeatures(size, num_features) T
    //    val theta: Row = List.fill(num_features+1)(2)
    val theta: Row = List.tabulate(num_features+1)(n => n + 2)
    (GenerateDependentFeatures(size, Xmat, theta), Xmat )
  }

  /*
    h(x) =  c + m1 x1 + m2 x2 + m3 x3 + ...
   */
  def HypothesisLinear(size: Int, theta: Row, X: Matrix): Row = {
    GenerateDependentFeatures(size, X, theta)
  }

  /*
    SumOf(
   */
  def PartialDerivs(distance: List[Double], X: Matrix, size: Int, num_features: Int): List[Double] = {
    var derivatives: Array[Double]  = Array.fill(num_features)(0)
    var i:Int = 0
    while(i < num_features){
      derivatives(i) = X(i).zip(distance).map{t:(Double, Double) => t._1 * t._2}.sum/size
      //      derivatives(i) = X(i).sum * distance(i) / size
      i = i + 1
    }
    //    println(distance)
    derivatives.toList
  }


  def PartialDerivativesAndCost(size: Int,theta: Row, X: Matrix,
                                Y: Row, hypo: Row => Matrix => Row):(Double,Row)={
    //h(x) =  c + m1 x1 + m2 x2 + m3 x3 + ...
    val hypothesis: List[Double] = hypo(theta)( X)
    //hypothesis(i) - y(i)
    val distance: List[Double] = hypothesis.zip(Y).map{t:(Double, Double) => t._1 - t._2}
    // ((hypothesis(i) - y(i)) * (hypothesis(i) - y(i)) / 2 * m
    val costSquared = distance.map(scala.math.pow(_, 2))
    val cost: Double = costSquared.sum / (2 * size)
    // (hypothesis(i) - y(i)) * x(i)/m
    val partial_derive: List[Double] = PartialDerivs(distance, X.T, size, theta.length)
    (cost, partial_derive)
  }

  def GradientDescent(i_var: Matrix, d_var: Row, size: Int, num_features: Int):List[Double] ={
    var theta: Array[Double]  = Array.fill(num_features+1){0.0}
    var (cost_func_last: Double, num_iter, alpha, m) = (0.0, 0, 0.000000003, size)
    var index: Int = 0
    var converged: Boolean = false
    val funName = (HypothesisLinear (_, _,_)).curried
    while(!converged){
      num_iter += 1
      //get cost function and partial derivatives
      val (cost_func, partial_derive) = PartialDerivativesAndCost(size, theta.toList, i_var, d_var, funName(size))
      index = index + 1
      //theta = theta_old - alpha * partial_derivative
      val zippedList  = theta.zip(partial_derive).toList
      theta = zippedList.map{t: (Double, Double) => t._1 - (alpha * t._2)}.toArray
      //      theta(0) = zippedList.head._1 - (alpha * 10 * zippedList.head._2)
      //      theta(1) = zippedList.head._1 - (alpha * zippedList.head._2)
      //      theta(2) = zippedList.head._1 - (alpha * 0.01 * zippedList.head._2)
      //      theta(3) = zippedList.head._1 - (alpha * 0.001 * zippedList.head._2)
      //      theta(4) = zippedList.head._1 - (alpha * 0.0001 * zippedList.head._2)
      println(theta.toList)
      println("-------")
      println(cost_func)
      if(cost_func > cost_func_last)
        alpha = alpha / 10
      if (cost_func < 0.000001)
        converged = true
      cost_func_last = cost_func
      //      println(math.abs(cost_func - cost_func_last))
    }
    theta.toList
  }

  def ScaleFeatures(i_var: Matrix, num_features: Int): Matrix = {
    val i_var_without_header: Matrix = i_var.tail
    var result: Matrix = List(List.fill(i_var.head.length)(1.0))
    var i: Int = 0
    while(i < num_features){
      val avg: Double = i_var_without_header(i).sum / i_var_without_header(i).length
      val range: Double = i_var_without_header(i).max - i_var_without_header(i).min
      if(range != 0)
        result = result ::: List(i_var_without_header(i).map(x => (x - avg)/range))
      else
        result = result ::: List(i_var_without_header(i).map((_ - avg)))
      i = i + 1
    }
    result
  }

  def GetToWork() {
    val size: Int = 300
    val num_features: Int = 4
    val (y: Row, x: Matrix) = GenerateTrainingSet(size, num_features)
    println("------Y--------")
    println(y)
    println("------X--------")
    println(x)
    //        val scaledFeatures = ScaleFeatures(x.T, num_features)
    val theta: List[Double] = GradientDescent(x, y, size, num_features)
    println("Output")
    println(theta)
  }
}

object Descent {
  def main(args: Array[String]) {
    val starter = new Descent()
    starter.GetToWork()
  }
}