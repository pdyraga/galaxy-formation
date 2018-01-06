package com.ontheserverside.galaxy

/**
  * Special type of an euclidean vector representing the position of a point P in space
  * in relation to a reference origin O. In our 2-dimensional space case, O is always (0, 0).
  */
case class PositionVector(
  headX: Double,
  headY: Double
) extends EuclideanVectorBase {
  val tailX = 0
  val tailY = 0
}