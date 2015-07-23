package Trainees

import Classes.MathHelperTypedefs._
import MathHelper.MatrixImpl._

import scala.collection.parallel.immutable.ParVector

/**
 * Created by Angeleyes on 7/21/2015.
 */
trait TrainingDataAPI{
  def GenerateTrainingSet(size: Int, numFeatures: Int): Matrix
}

class TrainingDataGen extends TrainingDataAPI{
  def GenerateIndependentFeatures(size: Int, num_features: Int): Matrix = {
    var i: Int = 1
    var result: Matrix = ParVector(ParVector.fill(size)(1))
    while(i <= num_features)
    {
      result = result ++ ParVector(ParVector.tabulate(size)(n=> (n+n*i+i).toDouble))
      i = i + 1
    }
    result
  }

  def GenerateDependentFeatures(size:Int, X: Matrix, Theta: Row): Row = {
    var Y: Row = ParVector()
    var i: Int = 0
    while(i < size)
    {
      Y = Y ++ ParVector(dotProd(Theta, X(i)))
      i = i + 1
    }
    Y
  }

  def GenerateTrainingSet(size:Int, num_features: Int): Matrix = {
    val Xmat: Matrix = GenerateIndependentFeatures(size, num_features) T
    val theta: Row = ParVector.tabulate(num_features+1)(n => n + 2)
    val Ymat: Row = GenerateDependentFeatures(size, Xmat, theta)
    println("----Used Theta----")
    println(theta)
    Ymat +: Xmat
  }

}


