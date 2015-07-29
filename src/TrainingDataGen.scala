package Trainees

import Classes.MathHelperTypedefs._
import MathHelper.MatrixImpl._

/**
 * Created by Angeleyes on 7/21/2015.
 */
trait TrainingDataAPI{
  def GenerateTrainingSet(size: Int, numFeatures: Int): Matrix
}

class TrainingDataGen extends TrainingDataAPI{
  def GenerateIndependentFeatures(size: Int, num_features: Int): Matrix = {
    var i: Int = 1
    var result: Matrix = Vector(Vector.fill(size)(1))
    while(i <= num_features)
    {
      result = result ++ Vector(Vector.tabulate(size)(n=> (n+n*i+i).toDouble))
      i = i + 1
    }
    result
  }

  def GenerateDependentFeatures(size:Int, X: Matrix, Theta: Row): Row = {
    var Y: Row = Vector()
    var i: Int = 0
    while(i < size)
    {
      Y = Y ++ Vector(dotProd(Theta, X(i)))
      i = i + 1
    }
    Y
  }

  def GenerateTrainingSet(size:Int, num_features: Int): Matrix = {
    val Xmat: Matrix = GenerateIndependentFeatures(size, num_features) T
//        val theta: Row = List.fill(num_features+1)(2)
    val theta: Row = Vector.tabulate(num_features+1)(n => n + 2)
    val Ymat: Row = GenerateDependentFeatures(size, Xmat, theta)
    Ymat +: Xmat
  }

}


