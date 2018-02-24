package com.ontheserverside.galaxy.profile

import com.ontheserverside.galaxy.Constants.{G, yearsToSeconds}
import com.ontheserverside.galaxy.{EuclideanVector, Point, Space}

object BlackHoleProfile extends Profile with UniformPointDistributionSupport {

  val stepDuration = yearsToSeconds(1000)

  private[this] val pointsCount = 4000

  private[this] val centralMass = 10e4

  private[this] val rMax = 5

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
