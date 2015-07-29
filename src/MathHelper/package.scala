package Classes
import scala.collection.immutable.Vector

/**
 * Created by psinha4 on 7/21/2015.
 */
package object MathHelperTypedefs {
  type Row = Vector[Double]
  def Row(xs: Double*) = Vector(xs: _*)
  type Matrix = Vector[Row]
  def Matrix(xs: Row*) = Vector(xs: _*)
}
