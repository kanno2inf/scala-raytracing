package raytracing.scene

import raytracing.material.Spectrum.BLACK
import raytracing.material.{Light, Spectrum}
import raytracing.math.Ray
import raytracing.obj.{Intersectable, Intersection}

import scala.collection.mutable.ListBuffer


object Scene {
  val DEPTH_MAX = 20 // 反射レイの打ち切り
}

// シーン
class Scene {
  val objList = new ListBuffer[Intersectable]
  val lightList = new ListBuffer[Light]
  var skyColor: Spectrum = BLACK

  // 形状追加
  def addIntersectable(obj: Intersectable): Unit = objList.append(obj)

  // 光源を追加
  def addLight(light: Light): Unit = lightList.append(light)

  // 最近傍の交点を求める
  def findNearestIntersection(ray: Ray): Intersection = objList.map(_.intersect(ray)).minBy(_.t)
}
