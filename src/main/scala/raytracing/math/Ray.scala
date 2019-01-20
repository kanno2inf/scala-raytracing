package raytracing.math

object Ray {
  val EPSILON = 0.001f // レイを進ませるオフセット

  def apply(origin: Vec, dir: Vec): Ray = {
    val normalized_dir = dir.normalize
    // 計算誤差によりレイが物体内側に潜り込む場合があるので始点をEPSILONだけ進める。
    val added_origin = origin.add(normalized_dir.scale(EPSILON))
    new Ray(added_origin, normalized_dir)
  }
}

class Ray(val origin: Vec, val dir: Vec)
