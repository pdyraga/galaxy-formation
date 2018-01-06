package com.ontheserverside.galaxy

import org.specs2.mutable.Specification

class EuclideanVectorBaseSpec extends Specification {

  "EuclideanVectorBase" should {

    "compute magnitude" in {
      new EuclideanVectorBase {
        val tailX = 0
        val tailY = 0
        val headX = 16
        val headY = 0
      }.magnitude must_== 16

      new EuclideanVectorBase {
        val tailX = 0
        val tailY = 0
        val headX = 0
        val headY = 6
      }.magnitude must_== 6

      new EuclideanVectorBase {
        val tailX = 0
        val tailY = 0
        val headX = 4
        val headY = 6
      }.magnitude must beCloseTo(7.2111, 4.significantFigures)

      new EuclideanVectorBase {
        val tailX = 6
        val tailY = 12
        val headX = 5
        val headY = 6
      }.magnitude must beCloseTo(6.0827, 4.significantFigures)
    }
  }
}
