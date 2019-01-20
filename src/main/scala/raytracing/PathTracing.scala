package raytracing

import java.util.concurrent.Executors

import processing.core.PApplet
import processing.core.PApplet.{max, min, radians}
import raytracing.app.AppMain
import raytracing.material.{Material, Spectrum}
import raytracing.math.{Ray, Vec}
import raytracing.obj.{CheckedObj, Plane, Sphere}
import raytracing.scene.{Camera, PathTracingScene}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

object PathTracing extends AppMain[PathTracing]

class PathTracing extends PApplet {
  // レイトレーシングスレッド数
  val cores: Int = Runtime.getRuntime.availableProcessors
  val threadMax: Int = max(1, cores - 1) // メイン描画スレッドのために1コア残す
  implicit val context: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(threadMax))

  // 並列描画時の分割ピクセルサイズ
  lazy val parallelStep: Int = 8 //max(width, height) / threadMax

  val SAMPLES = 200

  val scene = new PathTracingScene()
  lazy val cam: Camera = Camera.lookAt(Vec(4, 1.5f, 6), Vec(0, 0, 0), Vec(0, 1, 0), radians(45), width, height)


  override def settings(): Unit = {
    size(512, 512)
    initScene()
  }

  def initScene(): Unit = {
    scene.skyColor = Spectrum(1, 1, 1)

    val mtl1 = Material(Spectrum(0.7f, 0.3f, 0.9f))
    scene.addIntersectable(Sphere(Vec(-2.2f, 0, 0), 1, mtl1))

    val mtl2 = Material(Spectrum(0.9f, 0.7f, 0.3f), reflective = 0.8f)
    scene.addIntersectable(Sphere(Vec(0, 0, 0), 1, mtl2))

    val mtl3 = Material(Spectrum(0.3f, 0.9f, 0.7f), refractive = 0.8f, refractiveIndex = 1.5f)
    scene.addIntersectable(Sphere(Vec(2.2f, 0, 0), 1, mtl3))

    // 光源
    val mtlLight = Material(Spectrum.BLACK, emissive = Spectrum(30f, 20f, 10f))
    scene.addIntersectable(Sphere(Vec(0, 4.0f, 0), 1, mtlLight))

    // チェック柄の床
    val mtlFloor1 = Material(Spectrum(0.9f, 0.9f, 0.9f))
    val mtlFloor2 = Material(Spectrum(0.4f, 0.4f, 0.4f))
    scene.addIntersectable(CheckedObj(Plane(Vec(0, -1, 0), Vec(0, 1, 0), mtlFloor1), 1, mtlFloor2))
  }

  // 1次レイを返す
  def primaryRay(x: Float, y: Float, rand: Random): Ray = cam.ray(
    x + rand.nextFloat() - 0.5f, //p.random(-0.5f, 0.5f),
    y + rand.nextFloat() - 0.5f, //p.random(-0.5f, 0.5f)
  )

  // ピクセルの色を計算
  def pixelColor(x: Int, y: Int, rand: Random): Int = {
    val sum = (0 until SAMPLES).map(_ => scene.trace(primaryRay(x, y, rand), 0, rand)).reduce((x, y) => x + y)
    sum.scale(1f / SAMPLES).toGammaColor
  }

  // レンダリングタスク姿勢
  def renderTasks: Seq[Future[Seq[(Int, Int, Int)]]] = {
    for (y <- 0 until height by parallelStep; x <- 0 until width by parallelStep) yield Future {
      val rand = new Random()
      for (py <- y until y + parallelStep; px <- x until x + parallelStep)
        yield (px, py, pixelColor(px, py, rand))
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
