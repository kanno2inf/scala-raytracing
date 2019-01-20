package raytracing.scene

import processing.core.PApplet.pow
import processing.core.PConstants._
import raytracing.material.Material.VACUUM_REFRACTIVE_INDEX
import raytracing.material.Spectrum.BLACK
import raytracing.material.{Material, Spectrum}
import raytracing.math.{Ray, Vec}
import raytracing.obj.Intersection

class RayTracingScene extends Scene {
  /**
    * レイトレーシング
    *
    * @param ray   レイ
    * @param depth トレース計算回数
    * @return トレーシングにより計算した色
    */
  def trace(ray: Ray, depth: Int): Spectrum = {
    if (Scene.DEPTH_MAX < depth) return BLACK // 指定回数を超えたら打ち切る
    val isect = findNearestIntersection(ray)
    if (!isect.hit) return BLACK

    val m = isect.material
    if (isect.n.dot(ray.dir) >= 0) { // 物体から出ていく場合、物体→真空
      val r = ray.dir.refract(isect.n.neg, m.refractiveIndex / VACUUM_REFRACTIVE_INDEX) // 屈折レイ
      return trace(Ray(isect.p, r), depth + 1)
    }

    val reflective = traceReflective(isect, ray, depth) // 鏡面反射
    val refractive = traceRefractive(isect, ray, depth) // 屈折
    val diffuse: Spectrum = traceDiffuse(isect) // 拡散
    reflective.add(refractive).add(diffuse)
  }

  // 鏡面反射
  def traceReflective(isect: Intersection, ray: Ray, depth: Int): Spectrum = {
    val mtl = isect.material
    val ks = mtl.reflective
    if (ks <= 0) return BLACK
    val r = ray.dir.reflect(isect.n)
    // 反射レイ
    val c = trace(Ray(isect.p, r), depth + 1)
    // フレネル反射
    val f0 = ks
    val f1 = 1 - ray.dir.normalize.neg.dot(isect.n)
    val fresnel = f0 + (1 - f0) * pow(f1, 5) // Schlick近似
    c.scale(fresnel).mul(mtl.diffuse)
  }

  // 屈折色
  def traceRefractive(isect: Intersection, ray: Ray, depth: Int): Spectrum = {
    val mtl = isect.material
    val kt = mtl.refractive
    if (kt <= 0) return BLACK
    // 物体に入射する場合、真空→物体
    val r = ray.dir.refract(isect.n, VACUUM_REFRACTIVE_INDEX / mtl.refractiveIndex)
    val c = trace(Ray(isect.p, r), depth + 1)
    c.scale(kt).mul(mtl.diffuse)
  }


  // 拡散反射色
  def traceDiffuse(isect: Intersection): Spectrum = {
    val ks = isect.material.reflective
    val kd = 1.0f - ks
    if (kd <= 0) return BLACK
    lighting(isect.p, isect.n, isect.material).scale(kd)
  }


  // 光源計算
  def lighting(p: Vec, n: Vec, m: Material): Spectrum =
    lightList.map(light => diffuseLighting(p, n, m.diffuse, light.pos, light.power)).reduce((x, y) => x + y)


  // 拡散反射光計算
  def diffuseLighting(p: Vec, n: Vec, diffuseColor: Spectrum, lightPos: Vec, lightPower: Spectrum): Spectrum = { // 交点からの光源ベクトル
    val v = lightPos.sub(p)
    val l = v.normalize
    val dot = n.dot(l)
    if (dot <= 0) return BLACK // 陰
    if (!visible(p, lightPos)) return BLACK // 交点と光源の間に遮るものがあれば影
    val r = v.len
    // 光源距離
    val factor = dot / (4 * PI * r * r)
    lightPower.scale(factor).mul(diffuseColor)
  }

  // ベクトルを遮るものが存在しない場合true
  def visible(org: Vec, target: Vec): Boolean = {
    val v = target.sub(org).normalize
    // シャドウレイ
    val shadowRay = Ray(org, v)
    val isObstruct = objList.exists(_.intersect(shadowRay).t < v.len) // 影レイを遮るオブジェクトが存在
    !isObstruct
  }
}
