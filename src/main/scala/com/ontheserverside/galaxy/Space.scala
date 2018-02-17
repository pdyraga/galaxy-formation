package com.ontheserverside.galaxy

import java.util.concurrent.ThreadLocalRandom

case class Point(
  position: EuclideanVector,
  velocity: EuclideanVector,
  mass: Double = 1.0
) {
  def distanceVector(anotherPoint: Point): EuclideanVector = {
    EuclideanVector(
      anotherPoint.position.x - this.position.x,
      anotherPoint.position.y - this.position.y
    )
  }
}

class Space(val points: Array[Point])

object Space {
  def generateHomogeneousSpace(
    pointsCount: Int,
    velocityFn: Double => Double,
    rMax: Double,
    centralMass: Option[Double] = None
  ): Space = {
    val centralMassPoint = centralMass.map(mass =>
      Point(EuclideanVector(0, 0), EuclideanVector(0,0), mass)
    )

    new Space(
      centralMassPoint.toArray ++
      Array.fill(pointsCount)(drawPoint(0, rMax, velocityFn))
    )
  }

  def generateSpaceFromDensityFn(
    densityFn: Double => Double,
    velocityFn: Double => Double,
    rMax: Double,
    step: Double = 0.01
  ) = {
    val generatedPoints = (for (r <- 0.0 until rMax by step) yield {
      val r2 = r + step

      val density = if (r == 0) densityFn(r2) else ((densityFn(r) + densityFn(r2)) / 2)
      val area = (Math.pow(r2, 2) - Math.pow(r, 2)) * Math.PI
      val numberOfPoints = (density * area).toInt

      Seq.fill(numberOfPoints)(drawPoint(r, r2, velocityFn))
    }).flatten.toArray

    new Space(generatedPoints)
  }
  
  def drawPoint(rMin: Double, rMax: Double, velocityFn: Double => Double): Point = {
    val random = ThreadLocalRandom.current()

    val r = random.nextDouble(0, 1)
    val φ = random.nextDouble(0, Math.PI * 2)

    val R = Math.sqrt((Math.pow(rMax, 2) - Math.pow(rMin, 2)) * r + Math.pow(rMin, 2))

    val positionVector = EuclideanVector(R * Math.cos(φ), R * Math.sin(φ))

    val velocity = velocityFn(positionVector.magnitude)

    val velocityVector = EuclideanVector(
      -1 * velocity * Math.sin(φ),
      velocity * Math.cos(φ)
    )

    new Point(positionVector, velocityVector)
  }
}