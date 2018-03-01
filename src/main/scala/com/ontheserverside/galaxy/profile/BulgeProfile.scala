package com.ontheserverside.galaxy.profile

import com.ontheserverside.galaxy.Constants.{G, yearsToSeconds}
import com.ontheserverside.galaxy.{Constants, Space}

object BulgeProfile extends Profile with DensityFunctionGenSupport {

  val stepDuration = yearsToSeconds(100)

  val softeningLength = 0.5e-5

  private[this] val rMax = 60

  private[this] val scaleLength = 0.5

  private[this] val totalMass = 10e6

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