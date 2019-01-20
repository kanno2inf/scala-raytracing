package raytracing.obj

import processing.core.PApplet.round
import raytracing.material.Material
import raytracing.math.Ray

case class CheckedObj(obj: Intersectable, gridWidth: Float, material2: Material) extends Intersectable {
  def intersect(ray: Ray): Intersection = {
    val isect = obj.intersect(ray)
    if (!isect.hit) return isect
    val i: Int = round(isect.p.x / gridWidth) + round(isect.p.y / gridWidth) + round(isect.p.z / gridWidth)
    if (i % 2 != 0) return isect
    Intersection(isect.t, isect.p, isect.n, material2)
  }
}
