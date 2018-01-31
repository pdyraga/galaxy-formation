package com.ontheserverside.galaxy

import java.util.concurrent.ThreadLocalRandom

import strawman.collection.immutable.ImmutableArray

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

trait Constants {
  /**
    *  Gravitational constant
    */
  val G = 6.67408E-11;

  /**
    *  Mass of the central point
    */
  val centralMass = 10E10 //333000 Earth vs Sun mass
}

class Space(val points: ImmutableArray[Point])

object Space extends Constants {
  def generateHomogeneousSpace(pointsCount: Int, rMax: Double): Space = {
    val random = ThreadLocalRandom.current()

    val centralMassPoint = Point(
      EuclideanVector(0, 0), EuclideanVector(0,0), centralMass
    )

    new Space(centralMassPoint +: ImmutableArray.fill(pointsCount) {
      val r = random.nextDouble(0, 1)
      val φ = random.nextDouble(0, Math.PI * 2)

      val positionVector = EuclideanVector(
        Math.sqrt(r) * Math.cos(φ) * rMax,
        Math.sqrt(r) * Math.sin(φ) * rMax
      )

      val velocity = Math.sqrt(
        (G * centralMass) / positionVector.magnitude
      )

      val velocityVector = EuclideanVector(
        -1 * velocity * Math.sin(φ),
        velocity * Math.cos(φ)
      )

      new Point(positionVector, velocityVector)
    })
  }
}