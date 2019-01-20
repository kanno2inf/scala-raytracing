package raytracing.obj

import raytracing.math.Ray

// 物体インターフェイス
trait Intersectable {
  def intersect(ray: Ray): Intersection
}
