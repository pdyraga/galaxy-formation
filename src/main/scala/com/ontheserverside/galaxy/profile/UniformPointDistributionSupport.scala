package com.ontheserverside.galaxy.profile

import java.util.concurrent.ThreadLocalRandom

import com.ontheserverside.galaxy.Point

trait UniformPointDistributionSupport {
  /**
    * Lets to generate uniform distribution of random points in a circular ring.
    *
    * @param rMin - radius of the smaller circle determining area of annulus
    * @param rMax - radius of the bigger circle determining area of annulus
    * @param velocityFn - function used to evaluate point's velocity; should accept a distance from pole
    *                   to `Point` as an argument.
    */
  def createPoint(rMin: Double, rMax: Double, velocityFn: Double => Double): Point = {
    val random = ThreadLocalRandom.current()

    val ρ = random.nextDouble(0, 1)
    val φ = random.nextDouble(0, Math.PI * 2)

    val R = Math.sqrt((Math.pow(rMax, 2) - Math.pow(rMin, 2)) * ρ + Math.pow(rMin, 2))

    Point.fromPolarCoordinates(ρ, φ, R, velocityFn)
  }
}