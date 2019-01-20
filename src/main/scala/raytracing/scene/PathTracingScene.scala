package raytracing.scene

import raytracing.material.Material.VACUUM_REFRACTIVE_INDEX
import raytracing.material.Spectrum.BLACK
import raytracing.material.{Material, Spectrum}
import raytracing.math.{Ray, Vec}

import scala.util.Random

class PathTracingScene extends Scene {
  /**
    * パストレース
    *
    * @param ray   レイ
    * @param depth トレース回数
    * @param rand  Randomオブジェクト
    * @return トレースを集計した色情報
    */
  def trace(ray: Ray, depth: Int, rand: Random = new Random()): Spectrum = {
    if (Scene.DEPTH_MAX < depth) return BLACK

    val isect = findNearestIntersection(ray)
    if (!isect.hit) return skyColor

    val m = isect.material
    val dot = isect.n.dot(ray.dir)

    if (dot < 0) {
      // 外部から進入するレイ
      val col = interactSurface(ray.dir, isect.p, isect.n, m, VACUUM_REFRACTIVE_INDEX / m.refractiveIndex, depth, rand)
      return col.add(m.emissive.scale(-dot))
    }

    // 内部から出ていくレイ
    interactSurface(ray.dir, isect.p, isect.n.neg, m, m.refractiveIndex / VACUUM_REFRACTIVE_INDEX, depth, rand)
  }

  /**
    * 物体の色を計算
    *
    * @param rayDir レイ方向
    * @param p      交点座標
    * @param n      交点法線
    * @param m      マテリアル
    * @param eta    相対屈折率
    * @param depth  現在のパストレース計算回数
    * @param rand   Randomオブジェクト
    * @return トレースを集計した色情報
    */
  def interactSurface(rayDir: Vec, p: Vec, n: Vec, m: Material, eta: Float, depth: Int, rand: Random): Spectrum = {
    val ks = m.reflective
    val kt = m.refractive

    val t = rand.nextFloat() //pa.random(0.0f, 1.0f)
    if (t < ks) {
      // 鏡面反射
      val r = rayDir.reflect(n)
      val c = trace(Ray(p, r), depth + 1, rand)
      return c * m.diffuse
    }
    if (t < ks + kt) {
      // 屈折
      val r = rayDir.refract(n, eta)
      val c = trace(Ray(p, r), depth + 1, rand)
      return c * m.diffuse
    }
    // 拡散
    val r = n.randomHemisphere(rand)
    val li = trace(Ray(p, r), depth + 1, rand)

    val fr = m.diffuse
    val factor = 2 * n.dot(r)
    li * fr * factor
  }
}
