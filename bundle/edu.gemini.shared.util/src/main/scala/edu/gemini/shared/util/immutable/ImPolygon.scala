package edu.gemini.shared.util.immutable

import java.awt.{Rectangle, Polygon, Shape}
import java.awt.geom._


/**
 * An immutable class representing a polygon as a collection of points.
 * Note that building the polygon point-by-point is possible through the addPoint method, but is not
 * efficient, and instead, the collection of points should be passed as a constructor argument.
 * @param points the points, in order, representing the vertices of the polygon, which is assumed to be closed
 *               and thus does not require the first point to also be the last.
 */
case class ImPolygon(points: List[(Double,Double)] = Nil) extends Shape {
  // Calculate the path.
  private val closedPath = {
    val pathOption = points.headOption.map {
      case(x,y) =>
        val path = new GeneralPath()
        path.moveTo(x,y)
        path
    }

    pathOption.foreach { path => {
      points.drop(1).foreach { case (x,y) => path.lineTo(x,y) } }
      path.closePath()
    }
    pathOption
  }

  // Calculate the bounds.
  private val bounds = closedPath.getOrElse(new GeneralPath()).getBounds

  override def getBounds: Rectangle =
    bounds

  override def getBounds2D: Rectangle2D =
    bounds.getBounds2D

  override def getPathIterator(at: AffineTransform): PathIterator =
    closedPath.map(_.getPathIterator(at)).orNull

  // Flatness is ignored because a polygon is already flat.
  override def getPathIterator(at: AffineTransform, flatness: Double): PathIterator =
    getPathIterator(at)

  override def contains(x: Double, y: Double): Boolean =
    !((points.length <= 2 || !bounds.contains(x, y)) || !closedPath.forall(_.contains(x, y)))

  override def contains(p: Point2D): Boolean =
    contains(p.getX, p.getY)

  override def contains(x: Double, y: Double, w: Double, h: Double): Boolean =
    !((points.isEmpty || !bounds.contains(x, y)) || !closedPath.forall(_.contains(x, y, w, h)))

  override def contains(r: Rectangle2D): Boolean =
    contains(r.getX, r.getY, r.getWidth, r.getHeight)

  override def intersects(x: Double, y: Double, w: Double, h: Double): Boolean =
    !((points.isEmpty || !bounds.contains(x, y)) || !closedPath.forall(_.intersects(x, y, w, h)))

  override def intersects(r: Rectangle2D): Boolean =
    intersects(r.getX, r.getY, r.getWidth, r.getHeight)

  def addPoint(x: Double, y: Double): ImPolygon =
    new ImPolygon(points ++ List((x,y)))

  def addPoint(p: Point2D): ImPolygon =
    addPoint(p.getX, p.getY)
}

object ImPolygon {
  def apply(rect: Rectangle2D): ImPolygon = {
    val points = for {
      x <- List(rect.getMinX, rect.getMaxX)
      y <- List(rect.getMinY, rect.getMaxY)
    } yield (x, y)
    new ImPolygon(points)
  }

  def apply(pol: Polygon): ImPolygon = {
    val points = for {
      x <- pol.xpoints.map(_.toDouble)
      y <- pol.ypoints.map(_.toDouble)
    } yield (x, y)
    new ImPolygon(points.toList)
  }
}