package raytracing.obj

import raytracing.material.Material
import raytracing.math.{Ray, Vec}

object Plane {
  def apply(p: Vec, n: Vec, material: Material): Plane = new Plane(p, n.normalize, material)
}

// 無限平面
class Plane(val p: Vec, val n: Vec, val material: Material) extends Intersectable {
  val d: Float = -p.dot(n)

  def intersect(ray: Ray): Intersection = {
    val v = n.dot(ray.dir)
    val t = -(n.dot(ray.origin) + d) / v
    if (t <= 0) return Intersection.NO_HIT_INTERSECTION
    val op = ray.origin.add(ray.dir.scale(t))
    Intersection(t, op, n, material)
  }
}
