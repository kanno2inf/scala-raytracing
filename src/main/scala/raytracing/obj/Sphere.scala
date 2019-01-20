package raytracing.obj

import processing.core.PApplet.{sq, sqrt}
import raytracing.material.Material
import raytracing.math.{Ray, Vec}

// 球
case class Sphere(center: Vec, radius: Float, material: Material) extends Intersectable {
  // レイとの交差判定Ray
  override def intersect(ray: Ray): Intersection = {
    val v = ray.origin - center
    val b = ray.dir.dot(v)
    val c = v.dot(v) - sq(radius)
    val d = b * b - c // 交点の判別式
    if (d < 0) return Intersection.NO_HIT_INTERSECTION

    // 交点までの距離を計算
    val s: Float = sqrt(d)
    var t: Float = -b - s // 近い交点
    if (t <= 0) t = -b + s // 後方にある場合は、遠い交点
    if (t <= 0) return Intersection.NO_HIT_INTERSECTION // 両点とも後方に存在
    val p = ray.origin + ray.dir.scale(t)
    val n = (p - center).normalize
    Intersection(t, p, n, material)
  }
}
