package MathHelper

/**
 * Created by Angeleyes on 7/20/2015.
 */

class MathHelpers(m: Matrix){



  def dotProd(v1:Row,v2:Row) =
    v1.zip( v2 ).
      map{ t:(Double,Double) => t._1 * t._2 }.
      reduceLeft(_ + _)

  def transpose(m:Matrix):Matrix =
    if(m.head.isEmpty) Nil
    else m.map(_.head) :: transpose(m.map(_.tail))

  def mXm( m1:Matrix, m2:Matrix ) =
    for( m1row <- m1 ) yield
    for( m2col <- transpose(m2) ) yield
    dotProd( m1row, m2col )

  def T = transpose(m)

  def *(that:MathHelpers) = mXm( this.m, that.m )
}

object MathHelpers{

  type Row = List[Double]
  def Row(xs: Int*) = List(xs: _*)
  type Matrix = List[Row]
  def Matrix(xs: Row*) = List(xs: _*)
  implicit def pimp(m: Matrix) = new MathHelpers(m)
}
