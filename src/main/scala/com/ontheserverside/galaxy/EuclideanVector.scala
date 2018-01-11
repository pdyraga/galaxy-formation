package com.ontheserverside.galaxy

case class EuclideanVector(x: Double, y: Double) {
  def magnitude: Double = Math.hypot(x, y)
}
