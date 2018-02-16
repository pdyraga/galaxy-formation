package com.ontheserverside.galaxy

import com.ontheserverside.galaxy.Constants.G

object Constants {
  /**
    *  Gravitational constant expressed in parsecs and Solar mass
    */
  val G = 4.3 * 10E-3 // pc/M_sun (km/s)^2

  /**
    * Transforms units: [km] to [pc]
    *
    * 1 pc = 3.086e+13 km
    */
  def kmToPc(km: Double): Double = km / 3.086E+13

  /**
    * 1 km = 3.24078e-14 pc
    */
  val kmPcRatio = 3.24078e-14
}

object BulgeProfile {
  def densityFn = (r: Double, totalMass: Double, scaleLength: Double) => {
    (totalMass * scaleLength) / (2 * Math.PI * r * Math.pow(r + scaleLength, 3))
  }

  def velocityFn = (r: Double, totalMass: Double, scaleLength: Double) => {
    Constants.kmToPc(Math.sqrt(G * totalMass * r) / (r + scaleLength))
  }
}