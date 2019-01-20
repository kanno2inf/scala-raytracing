package raytracing

import java.util.concurrent.Executors

import _root_.raytracing.material.{Light, Material, Spectrum}
import _root_.raytracing.obj.{CheckedObj, Plane, Sphere, TexturedObj}
import processing.core.PApplet
import processing.core.PApplet._
import raytracing.app.AppMain
import raytracing.math.{Ray, Vec}
import raytracing.scene.{Camera, RayTracingScene}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}


object RayTracing extends AppMain[RayTracing]

class RayTracing extends PApplet {
  // レイトレーシングスレッド数
  val cores: Int = Runtime.getRuntime.availableProcessors
  val threadMax: Int = max(1, cores - 1) // メイン描画スレッドのために1コア残す
  implicit val context: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadMax))

  // 並列描画時の分割ピクセルサイズ
  lazy val parallelStep: Int = 8

  val scene = new RayTracingScene()
  lazy val cam: Camera = Camera.lookAt(Vec(0, 0, 6), Vec(0, 0, 0), Vec(0, 1, 0), radians(50), width, height)

  override def settings(): Unit = {
    size(512, 512)
    initScene()
  }

  def initScene(): Unit = {
    scene.addIntersectable(Sphere(Vec(-2, 0, 0), 0.8f, Material(Spectrum(0.9f, 0.1f, 0.5f))))
    scene.addIntersectable(Sphere(Vec(0, 0, 0), 1, Material(Spectrum(0.1f, 0.9f, 0.5f), Spectrum.BLACK, 0.1f, 0.9f, 1.6f)))
    scene.addIntersectable(Sphere(Vec(2, 0, 0), 0.8f, Material(Spectrum(0.1f, 0.5f, 0.9f))))
    // チェック柄の床
    val mtlFloor1 = Material(Spectrum(0.5f, 0.5f, 0.5f), reflective = 0.1f)
    val mtlFloor2 = Material(Spectrum(0.2f, 0.2f, 0.2f), reflective = 0.1f)
    // 床をシーンに追加する
    scene.addIntersectable(CheckedObj(Plane(Vec(0, -1, 0), Vec(0, 1, 0), mtlFloor1), 1, mtlFloor2))
    // 猫柄の壁
    val mtlWall1 = Material(Spectrum(0.5f, 0.5f, 0.5f))
    scene.addIntersectable(TexturedObj(
      Plane(Vec(0, 0, -5), Vec(0, 0, 1), mtlWall1),
      loadImage("img/sakura.png"),
      10, Vec(-5, -4, 0), Vec(1, 0, 0), Vec(0, 1, 0), this))
    // 点光源2つをシーンに追加する
    scene.addLight(Light(Vec(100, 100, 100), Spectrum(400000, 100000, 400000)))
    scene.addLight(Light(Vec(-100, 100, 100), Spectrum(100000, 400000, 100000)))
  }

  // 1次レイを返す
  def primaryRay(x: Float, y: Float): Ray = cam.ray(x + 0.5f, y + 0.5f)

  // ピクセルの色を計算
  def pixelColor(x: Int, y: Int): Int = {
    val ray = primaryRay(x, y)
    scene.trace(ray, 0).toColor
  }

  // レンダリングタスク生成
  def renderTasks: Seq[Future[Seq[(Int, Int, Int)]]] = {
    for (y <- 0 until height by parallelStep; x <- 0 until width by parallelStep) yield Future {
      for (py <- y until y + parallelStep; px <- x until x + parallelStep)
        yield (px, py, pixelColor(px, py))
    }
  }

  lazy val tasks: Iterator[Future[Seq[(Int, Int, Int)]]] = renderTasks.toIterator

  override def draw(): Unit = {
    // 一定タスクごとに描画
    val restTasks = tasks.take(min(width, height) / parallelStep)
    for (task <- restTasks) {
      val colors = Await.result(task, Duration.Inf)
      for ((x, y, color) <- colors) set(x, y, color)
    }
    if (!tasks.hasNext) noLoop() // 計算完了したら描画を停止
  }
}
