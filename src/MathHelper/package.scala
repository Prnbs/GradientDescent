package Classes

/**
 * Created by psinha4 on 7/21/2015.
 */
package object MathHelperTypedefs {
  type Row = List[Double]
  def Row(xs: Int*) = List(xs: _*)
  type Matrix = List[Row]
  def Matrix(xs: Row*) = List(xs: _*)
}
