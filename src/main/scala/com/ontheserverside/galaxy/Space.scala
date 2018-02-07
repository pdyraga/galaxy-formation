package com.ontheserverside.galaxy

import java.util.concurrent.ThreadLocalRandom

import Constants._

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
  def generateHomogeneousSpace(pointsCount: Int, rMax: Double): Space = {
    val centralMassPoint = Point(
      EuclideanVector(0, 0), EuclideanVector(0,0), centralMass
    )

    new Space(centralMassPoint +: Array.fill(pointsCount)(drawPoint(rMax/2, rMax)))
  }

  def drawPoint(rMin: Double, rMax: Double): Point = {
    val random = ThreadLocalRandom.current()

    val r = random.nextDouble(0, 1)
    val φ = random.nextDouble(0, Math.PI * 2)

    val R = Math.sqrt((Math.pow(rMax, 2) - Math.pow(rMin, 2)) * r + Math.pow(rMin, 2))

    val positionVector = EuclideanVector(R * Math.cos(φ), R * Math.sin(φ))

    val velocity = Math.sqrt(
      (G * centralMass) / positionVector.magnitude
    )

    val velocityVector = EuclideanVector(
      -1 * velocity * Math.sin(φ),
      velocity * Math.cos(φ)
    )

    new Point(positionVector, velocityVector)
  }
}