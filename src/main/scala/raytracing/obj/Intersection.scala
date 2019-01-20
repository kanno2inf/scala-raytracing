package raytracing.obj

import raytracing.material.Material
import raytracing.math.Vec

object Intersection {
  val NO_HIT: Float = Float.PositiveInfinity // 交差していない
  val NO_HIT_INTERSECTION: Intersection = new Intersection(NO_HIT, Vec(), Vec(), Material())
}

case class Intersection(t: Float, p: Vec, n: Vec, material: Material) {
  def hit: Boolean = this.t != Intersection.NO_HIT
}
