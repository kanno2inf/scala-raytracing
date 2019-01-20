package raytracing.scene

import processing.core.PApplet._
import raytracing.math.{Ray, Vec}

object Camera {
  def lookAt(eye: Vec, target: Vec, up: Vec, fov: Float, width: Int, height: Int): Camera = {
    // 投影面の距離
    val imagePlane = (height / 2f) / tan(fov / 2f)
    // 注視点方向、単位ベクトル
    val v = (target - eye).normalize
    // X軸、Y軸単位ベクトル計算
    val axisX = v.cross(up).normalize
    val axisY = v.cross(axisX)
    // 投影面ベクトル
    val center = v.scale(imagePlane)
    // 投影面の左上へのベクトル
    val origin = center - (axisX.scale(0.5f * width) + axisY.scale(0.5f * height))
    new Camera(eye, origin, axisX, axisY)
  }
}

class Camera(eye: Vec, origin: Vec, axisX: Vec, axisY: Vec) {
  def ray(x: Float, y: Float): Ray = { // 投影面上の位置を計算
    val p = origin.add(axisX.scale(x)).add(axisY.scale(y))
    val dir = p.normalize // レイ方向
    Ray(eye, dir)
  }
}
