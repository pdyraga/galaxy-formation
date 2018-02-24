package com.ontheserverside.galaxy

import java.util.concurrent.ThreadLocalRandom

import com.ontheserverside.galaxy.Constants._

trait Profile {
  def generateSpace: Space

  def stepDuration: Double

  //TODO: move here softening factor
}

trait UniformPointDistributionSupport {
  /**
    * Lets to generate uniform distribution of random points in a circular ring.
    *
    * @param rMin - radius of the smaller circle determining area of annulus
    * @param rMax - radius of the bigger circle determining area of annulus
    * @param velocityFn - function used to evaluate point's velocity; should accept a distance from pole
    *                   to `Point` as an argument.
    */
  def createPoint(rMin: Double, rMax: Double, velocityFn: Double => Double): Point = {
    val random = ThreadLocalRandom.current()

    val ρ = random.nextDouble(0, 1)
    val φ = random.nextDouble(0, Math.PI * 2)

    val R = Math.sqrt((Math.pow(rMax, 2) - Math.pow(rMin, 2)) * ρ + Math.pow(rMin, 2))

    Point.fromPolarCoordinates(ρ, φ, R, velocityFn)
  }
}

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

object CartwheelProfile extends Profile {

  val stepDuration = yearsToSeconds(100)

  override def generateSpace: Space = {
    val pointsCount = 5000
    val ε = 1.0

    new Space(Array.fill(pointsCount) {
      val random = ThreadLocalRandom.current()

      val ρ = random.nextDouble(0, 1)
      val φ = random.nextDouble(0, Math.PI * 2)

      Point.fromPolarCoordinates(ρ, φ, 50 * ρ)
    })
  }
}


object BlackHoleProfile extends Profile with UniformPointDistributionSupport {
  val rMax = 5

  val stepDuration = yearsToSeconds(1000)

  val pointsCount = 4000

  val centralMass = 10e4

  override def generateSpace: Space = {
    val centralMassPoint = Point(EuclideanVector(0, 0), EuclideanVector(0,0), centralMass)
    new Space(centralMassPoint +: Array.fill(pointsCount)(
      createPoint(0, rMax, velocity))
    )
  }

  private[this] def velocity(r: Double): Double = {
    Math.sqrt(G * centralMass / r)
  }
}

object BulgeProfile extends Profile with DensityFunctionGenSupport {
  val rMax = 5

  val scaleLength = 0.5

  val stepDuration = yearsToSeconds(100)

  val totalMass = 10e4

  override def generateSpace: Space = {
    generateSpaceFromDensityFn(density, velocity, rMax)
  }

  private[this] def density(r: Double) =  {
    (totalMass * scaleLength) / (2 * Math.PI * r * Math.pow(r + scaleLength, 3))
  }

  private[this] def velocity(r: Double) = {
    Constants.kmToPc(Math.sqrt(G * totalMass * r) / (r + scaleLength))
  }
}