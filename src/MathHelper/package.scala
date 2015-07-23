package Classes
import scala.collection.parallel.immutable.ParVector

/**
 * Created by psinha4 on 7/21/2015.
 */
package object MathHelperTypedefs {
  type Row = ParVector[Double]
  def Row(xs: Double*) = ParVector(xs: _*)
  type Matrix = ParVector[Row]
  def Matrix(xs: Row*) = ParVector(xs: _*)
}
