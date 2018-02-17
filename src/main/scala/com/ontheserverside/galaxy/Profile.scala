package com.ontheserverside.galaxy

import com.ontheserverside.galaxy.Constants.G

trait Profile {
  def generateSpace: Space

  def rMax: Double
  def stepDuration: Double
}

object RingProfile extends Profile {
  val rMax = 5

  val stepDuration = 100 * 3.154e+7 // 100 years

  val pointsCount = 5000

  val centralMass = 10E5

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

  val stepDuration = 100 * 3.154e+7 // 100 years

  val totalMass = 5000

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