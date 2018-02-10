package com.ontheserverside.galaxy

import com.ontheserverside.galaxy.Constants.G

object Constants {
  /**
    *  Gravitational constant expressed in parsecs and Solar mass
    */
  val G = 4.3 * 10E-3 // pc/M_sun (km/s)^2
}

object BulgeProfile {
  def densityFn = (r: Double, totalMass: Double, scaleLength: Double) => {
    if (r > 0) (totalMass / 2 * Math.PI) * (scaleLength / r) / Math.pow(r + scaleLength, 3)
    else 0
  }

  def velocityFn = (r: Double, totalMass: Double, scaleLength: Double) => {
    Math.sqrt(G * totalMass * r) / (r + scaleLength)
  }
}