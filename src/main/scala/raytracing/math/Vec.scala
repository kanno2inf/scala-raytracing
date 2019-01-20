package raytracing.math

import processing.core.PApplet._

import scala.util.Random

object Vec {
  def apply(): Vec = new Vec(0, 0, 0)
}

case class Vec(x: Float, y: Float, z: Float) {

  def add(v: Vec) = Vec(x + v.x, y + v.y, z + v.z)

  def +(v: Vec): Vec = add(v)

  def sub(v: Vec): Vec = Vec(x - v.x, y - v.y, z - v.z)

  def -(v: Vec): Vec = sub(v)

  def scale(s: Float): Vec = Vec(x * s, y * s, z * s)

  //  def *(s: Float): Vec = scale(s)

  def neg = Vec(-x, -y, -z)

  def reflect(n: Vec): Vec = sub(n.scale(2 * dot(n)))

  // 法線、屈折率比
  def refract(n: Vec, eta: Float): Vec = {
    val dot_n = dot(n)
    val d = 1.0f - sq(eta) * (1.0f - sq(dot_n))
    if (d <= 0) return reflect(n) // 全反射
    // 屈折
    val a = sub(n.scale(dot_n)).scale(eta)
    val b = n.scale(sqrt(d))
    a.sub(b)
  }

  // 半球状のランダムな方向を選択
  def randomHemisphere(r: Random): Vec = {
    def randVec: Vec = Vec(
      r.nextFloat() * 2 - 1f,
      r.nextFloat() * 2 - 1f,
      r.nextFloat() * 2 - 1f
    )

    val dir: Vec = (0 until 50).map(_ => randVec).find(_.len < 1.0f).getOrElse(randVec)
    // 正規化
    val normalizedDir = dir.normalize
    if (dot(normalizedDir) >= 0) return normalizedDir
    normalizedDir.neg // 法線方向の半球上に合わせる
  }

  def len: Float = sqrt(sq(x) + sq(y) + sq(z))

  def normalize: Vec = scale(1.0f / len)

  def dot(v: Vec): Float = x * v.x + y * v.y + z * v.z

  def cross(v: Vec): Vec = Vec(y * v.z - v.y * z, z * v.x - v.z * x, x * v.y - v.x * y)

  def toSeq: Seq[Float] = Seq(x, y, z)

  override def toString: String = s"Vec($x,$y,$z)"
}
