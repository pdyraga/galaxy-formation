package com.ontheserverside.galaxy

object Constants {
  /**
    *  Gravitational constant expressed in parsecs [pc] and Solar mass [M_sun]
    */
  val G = 4.3 * 10e-3 // pc/M_sun (km/s)^2

  /**
    * Transforms units: [km] to [pc]
    *
    * 1 [pc] = 3.086e+13 [km]
    */
  def kmToPc(km: Double): Double = km / 3.086e+13

  /**
    * 1 [km] = 3.24078e-14 [pc]
    */
  val kmPcRatio = 3.24078e-14

  def yearsToSeconds(years: Double): Double = {
    years * 3.154e+7
  }
}
