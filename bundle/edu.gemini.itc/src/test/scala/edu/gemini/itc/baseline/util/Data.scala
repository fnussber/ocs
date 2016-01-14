package edu.gemini.itc.baseline.util

import edu.gemini.spModel.core.AlmostEqual._
import edu.gemini.itc.shared.SpcSeriesData

// A very bare bones data object for sampling stuff.
final case class Data(values: Array[(Double, Double)]) {

  def x(i: Int) = values(i)._1
  def y(i: Int) = values(i)._2

  lazy val xValues = values.map(_._1).toList
  lazy val yValues = values.map(_._2).toList

  def compress: Data =
    this.elimZeros.resample(2) // TODO: resample with 2nm, allow 1nm in case total amount of data is manageable

  // eliminate repeating zero values
  private def elimZeros: Data =
    copy(values = values.iterator.sliding(3,2).flatMap {
      case Seq((_,0),(_,0),(_,0)) => Seq()
      case Seq(a,b,_)             => Seq(a,b)
      case s                      => s
    }.toArray)

  // resample to 2nm (some data is sampled down to 0.2nm precision), TODO: pass desired decision instead of calculating the value?
  // TODO: use squants for wavelength values?? (note: some charts use pixels here..)
  private def resample(nm: Int): Data = {

    def combine = math.max(1,(nm/math.abs(x(0)-x(1))).toInt)

    def avg(s: Iterable[(Double, Double)]): Seq[(Double, Double)] =
      Seq(s.fold((0.0, 0.0))({ case (a, b) => (a._1 + b._1, a._2 + b._2) })).map { case (x, y) => (x / combine, y / combine) } // TODO: simplify

    // TODO: deal with case that eliminating zeroes entirely wiped out the data!
    if (values.length < 2) return this

    copy(values = values.sliding(combine, combine).flatMap {
      case s if s.exists { case (x, y) => y == 0 }  => s        // do NOT fiddle with 0 values!
      case s                                        => avg(s)
    }.toArray)

  }

  def toArrays: Array[Array[Double]] = {
    val a: Array[Array[Double]] = new Array(2)
    a(0) = xValues.toArray
    a(1) = yValues.toArray
    a
  }

  def ~=(other: Data): Boolean =
    (xValues ~= other.xValues) && (yValues ~= other.yValues)

}

object Data {
  def fromSeries(s: SpcSeriesData) = {
    require(s.xValues.length == s.yValues.length, "same amount of x and y values required")
    require(s.xValues.length >= 2)
    new Data(s.xValues.zip(s.yValues))
  }

  def apply(data: Array[Array[Double]]): Data = {
    require(data.length == 2)
    require(data(0).length == data(1).length)
    Data(data(0).zip(data(1)))
  }
}
