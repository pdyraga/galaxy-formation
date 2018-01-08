package com.ontheserverside.galaxy

import java.util.concurrent.ThreadLocalRandom

import strawman.collection.immutable.ImmutableArray

case class Point(
  positionVector: PositionVector,
  velocityVector: EuclideanVector,
  mass: Double = 1.0
) {
  def distanceVector(anotherPoint: Point): EuclideanVector = {
    EuclideanVector(
      positionVector.headX,
      positionVector.headY,
      anotherPoint.positionVector.headX,
      anotherPoint.positionVector.headY
    )
  }
}

trait Constants {
  /**
    *  Gravitational constant
    */
  val G = 6.67408E-11;

  /**
    *  Mass of the central point
    */
  val centralMass = 333000 // Earth vs Sun mass
}

class Space(val points: ImmutableArray[Point])

object Space extends Constants {
  def generateHomogeneousSpace(pointsCount: Int, rMax: Int = 10): Space = {
    val random = ThreadLocalRandom.current()

    val centralMassPoint = Point(
      PositionVector(0, 0), EuclideanVector(0,0,0,0), centralMass
    )

    new Space(centralMassPoint +: ImmutableArray.fill(pointsCount) {
      val r = random.nextDouble(0, 1)
      val φ = random.nextDouble(0, Math.PI * 2)

      val positionVector = PositionVector(
        Math.sqrt(r) * Math.cos(φ) * rMax,
        Math.sqrt(r) * Math.sin(φ) * rMax
      )

      val velocity = Math.sqrt(
        (G * centralMass) / positionVector.magnitude
      )

      val velocityVector = EuclideanVector(
        positionVector.headX,
        positionVector.headY,
        velocity * Math.sin(φ),
        velocity * Math.cos(φ)
      )

      new Point(positionVector, velocityVector)
    })
  }
}