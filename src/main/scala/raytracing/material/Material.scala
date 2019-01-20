package raytracing.material

object Material {
  val VACUUM_REFRACTIVE_INDEX = 1.0f // 真空中の屈折率

  def apply(): Material = new Material(Spectrum())
}

case class Material(diffuse: Spectrum,
                    emissive: Spectrum = Spectrum.BLACK, // 発光色
                    reflective: Float = 0, // 反射率
                    refractive: Float = 0, // 屈折色率
                    refractiveIndex: Float = 1 // 屈折率
                   )
