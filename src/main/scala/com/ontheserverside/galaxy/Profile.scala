package com.ontheserverside.galaxy

import com.ontheserverside.galaxy.Constants._

trait Profile {
  def generateSpace: Space

  def rMax: Double
  def stepDuration: Double
}

object RingProfile extends Profile {
  val rMax = 5

  val stepDuration = yearsToSeconds(1000)

  val pointsCount = 5000

  val centralMass = 10e4

  override def generateSpace: Space = {
    Space.generateHomogeneousSpace(pointsCount, velocity, rMax, Some(centralMass))
  }

  def velocity(r: Double): Double = {
    Math.sqrt(G * centralMass / r)
  }
}

object BulgeProfile extends Profile {
  val rMax = 5

  val scaleLength = 0.5

  val stepDuration = yearsToSeconds(100)

  val totalMass = 10e4

  override def generateSpace: Space = {
    Space.generateSpaceFromDensityFn(density, velocity, rMax)
  }

  def density(r: Double) =  {
    (totalMass * scaleLength) / (2 * Math.PI * r * Math.pow(r + scaleLength, 3))
  }

  def velocity(r: Double) = {
    Constants.kmToPc(Math.sqrt(G * totalMass * r) / (r + scaleLength))
  }
}