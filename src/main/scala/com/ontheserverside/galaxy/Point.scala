package com.ontheserverside.galaxy

case class Point(
  position: EuclideanVector,
  velocity: EuclideanVector,
  mass: Double = 1.0 // [M_sun]
) {
  def distanceVector(anotherPoint: Point): EuclideanVector = {
    EuclideanVector(
      anotherPoint.position.x - this.position.x,
      anotherPoint.position.y - this.position.y
    )
  }
}

object Point {
  /**
    * Creates an instance of `Point` from provided polar coordinates and assigns it a provided `velocity`.
    *
    * Created `Point` has a mass of 1 Solar mass.
    *
    * @param ρ - radial coordinate (distance from pole)
    * @param φ - angle in radians
    * @param velocity - point's velocity
    */
  def fromPolarCoordinates(ρ: Double, φ: Double, velocity: Double): Point = {
    fromPolarCoordinates(ρ, φ, 1.0, _ => velocity)
  }

  /**
    * Creates an instance of `Point` from provided polar coordinates.
    *
    * Requires to provide an additional `R` scale factor which is used to scale up / scale down cartesian
    * coordinates of the `Point`. This technique is useful when generating uniformly distributed random points
    * in a circular ring.
    *
    * Uses `velocityFn` to evaluate velocity of `Point` passing as an argument distance from pole to the `Point`.
    *
    * Created `Point` has a mass of 1 Solar mass.
    *
    * @param ρ - radial coordinate (distance from pole)
    * @param φ - angle in radians
    * @param R - scale factor of the point's cartesian coordinates
    * @param velocityFn - function used to evaluate point's velocity; should accept a distance from pole
    *                   to `Point` as an argument.
    */
  def fromPolarCoordinates(ρ: Double, φ: Double, R: Double, velocityFn: Double => Double): Point = {
    fromPolarCoordinates(ρ, φ, R, R, velocityFn)
  }

  /**
    * Creates an instance of `Point` from the provided polar coordinates.
    *
    * Requires to provide an additional εx and εy scale factors used to scale up / scale down cartesian coordinates
    * of the `Point`. This technique is useful when generating elliptical distributions or uniformly distributed random
    * points in a circular ring.
    *
    * Uses `velocityFn` to evaluate velocity of `Point` passing as an argument distance from pole to the `Point`.
    *
    * Created `Point` has a mass of 1 Solar mass.
    *
    * @param ρ - radial coordinate (distance from pole)
    * @param φ - angle in radians
    * @param εx - scale factor of X point's cartesian coordinate
    * @param εy - scale factor of Y point's cartesian coordinate
    * @param velocityFn - function used to evaluate point's velocity; should accept a distance from pole
    *                   to `Point` as an argument.
    */
  def fromPolarCoordinates(ρ: Double, φ: Double, εx: Double, εy: Double, velocityFn: Double => Double): Point = {
    val positionVector = EuclideanVector(
      ρ * εx * Math.cos(φ),
      ρ * εy * Math.sin(φ)
    )

    val velocity = velocityFn(positionVector.magnitude)

    val velocityVector = EuclideanVector(
      -1 * velocity * Math.sin(φ),
      velocity * Math.cos(φ)
    )

    new Point(positionVector, velocityVector)
  }
}

