package raytracing.material

import processing.core.PApplet
import processing.core.PApplet._

object Spectrum {
  val BLACK = apply(0, 0, 0)
  val COLOR_SKY = Spectrum(0.7f, 0.7f, 0.7f)

  val DISPLAY_GAMMA = 2.2f

  val p = new PApplet()

  def apply(): Spectrum = BLACK
}

case class Spectrum(r: Float, g: Float, b: Float) {

  def add(v: Spectrum): Spectrum = Spectrum(r + v.r, g + v.g, b + v.b)

  def +(v: Spectrum): Spectrum = add(v)

  def sub(v: Spectrum): Spectrum = Spectrum(r - v.r, g - v.g, b - v.b)

  def -(v: Spectrum): Spectrum = sub(v)

  def mul(v: Spectrum): Spectrum = Spectrum(r * v.r, g * v.g, b * v.b)

  def *(v: Spectrum): Spectrum = mul(v)

  def scale(s: Float): Spectrum = Spectrum(r * s, g * s, b * s)

  def *(s: Float): Spectrum = scale(s)

  // Int Colorに変換
  def minToInt(x: Float): Int = min(x * 255, 255).toInt

  def toColor: Int = Spectrum.p.color(minToInt(r), minToInt(g), minToInt(b))

  def gamma(x: Float): Int = minToInt(pow(x, 1.0f / Spectrum.DISPLAY_GAMMA))

  // ガンマ調整した色
  def toGammaColor: Int = Spectrum.p.color(gamma(r), gamma(g), gamma(b))
}
