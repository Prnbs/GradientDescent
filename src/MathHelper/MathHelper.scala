package MathHelper

/**
 * Created by Angeleyes on 7/20/2015.
 */
import Classes.MathHelperTypedefs._
import scala.collection.immutable.Vector

class MatrixImpl(m: Matrix){
  val data: Matrix = m

  def transpose(m: Matrix):Matrix =
    if(m.head.isEmpty) Vector()
    else m.map(_.head) +: transpose(m.map(_.tail))

  def mXm( m1: Matrix, m2: Matrix ) =
    for( m1row <- m1 ) yield
      for( m2col <- transpose(m2) ) yield
      MatrixImpl.dotProd( m1row, m2col )

  def T = transpose(m)

  def *(that: MatrixImpl) = mXm( this.m, that.data )
}

object MatrixImpl{
  implicit def pimp(m: Matrix) = new MatrixImpl(m)

  def dotProd(v1:Row,v2:Row) =
    v1.zip( v2 ).
      map{ t:(Double,Double) => t._1 * t._2 }.sum
}