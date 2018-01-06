package com.ontheserverside.galaxy

trait EuclideanVectorBase {
  def tailX: Double
  def tailY: Double
  def headX: Double
  def headY: Double

  def magnitude: Double = Math.hypot(headX - tailX, headY - tailY)
}
