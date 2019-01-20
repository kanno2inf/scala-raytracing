package raytracing.app

import processing.core.PApplet

import scala.reflect.ClassTag

/**
  * Processingアプリエントリークラス
  * 継承したオブジェクトを定義する
  *
  * @tparam P PAppletを継承したクラス
  */
class AppMain[P <: PApplet](implicit tag: ClassTag[P]) {
  def main(args: Array[String]): Unit = PApplet.main(tag.runtimeClass)
}
