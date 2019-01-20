package raytracing.obj

import processing.core.PApplet.floor
import processing.core.{PApplet, PImage}
import raytracing.material.{Material, Spectrum}
import raytracing.math.{Ray, Vec}

/**
  * @param obj    // 交差物体
  * @param image  // 画像
  * @param size   // テクスチャサイズ
  * @param origin // テクスチャの原点
  * @param uDir   // u座標方向
  * @param vDir   // v座標方向
  */
case class TexturedObj(obj: Intersectable, image: PImage, size: Float, origin: Vec, uDir: Vec, vDir: Vec, p: PApplet)
  extends Intersectable {

  override def intersect(ray: Ray): Intersection = {
    val isect = obj.intersect(ray)
    if (!isect.hit) return isect

    val su = (isect.p - origin).dot(uDir) / size
    val sv = -(isect.p - origin).dot(vDir) / size
    val u = floor((su - floor(su)) * image.width) // 0～1の範囲にWRAPして参照
    val v = floor((sv - floor(sv)) * image.height)

    val c = image.get(u.toInt, v.toInt)
    val tc = Spectrum(p.red(c), p.green(c), p.blue(c)).scale(1.0f / 255f)
    val isect_mtl = isect.material
    val mtl = Material(tc * isect_mtl.diffuse, isect_mtl.emissive, isect_mtl.reflective, isect_mtl.refractive, isect_mtl.refractiveIndex)
    Intersection(isect.t, isect.p, isect.n, mtl)
  }
}
