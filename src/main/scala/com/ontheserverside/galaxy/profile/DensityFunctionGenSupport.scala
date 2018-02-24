package com.ontheserverside.galaxy.profile

import com.ontheserverside.galaxy.Space

trait DensityFunctionGenSupport extends UniformPointDistributionSupport {
  /**
    * Generates `Space` with density described by the provided `densityFn`.
    *
    * The algorithm generates a set of circular rings with a width equal to `step` parameter.
    * Each ring is filled with `Point`s according to the provided `densityFn`.
    * Ring's density is computed from the mean density at its inner and outer boundaries and `Point`s are uniformly
    * distributed inside the ring.
    *
    * The algorithm generates rings until the `rMax` distance from the pole.
    *
    * Bear in mind that this is just an approximation and `step` value must be individually
    * adjusted for each `densityFn` and `rMax`.
    *
    * Each generated `Point` has a velocity computed from the provided `velocityFn` accepting a distance from
    * pole to the `Point` as an argument.
    */
  def generateSpaceFromDensityFn(
    densityFn: Double => Double,
    velocityFn: Double => Double,
    rMax: Double,
    step: Double = 0.01
  ): Space = {
    val generatedPoints = (for (r <- 0.0 until rMax by step) yield {
      val r2 = r + step

      val density = if (r == 0) densityFn(r2) else ((densityFn(r) + densityFn(r2)) / 2)
      val area = (Math.pow(r2, 2) - Math.pow(r, 2)) * Math.PI
      val numberOfPoints = (density * area).toInt

      Seq.fill(numberOfPoints)(createPoint(r, r2, velocityFn))
    }).flatten.toArray

    new Space(generatedPoints)
  }
}