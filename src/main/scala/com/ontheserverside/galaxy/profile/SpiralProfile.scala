package com.ontheserverside.galaxy.profile

import java.util.concurrent.ThreadLocalRandom

import com.ontheserverside.galaxy.Constants.yearsToSeconds
import com.ontheserverside.galaxy.{Point, Space}

object SpiralProfile extends Profile {
  val stepDuration = yearsToSeconds(50)

  val softeningLength = 0.5e-5

  override def generateSpace: Space = {
    val pointsCount = 5000
    val εx = 1.0
    val εy = 0.25

    new Space(Array.fill(pointsCount) {
      val random = ThreadLocalRandom.current()

      val ρ = random.nextDouble(0, 0.25)
      val φ = random.nextDouble(0, Math.PI * 2)

      Point.fromPolarCoordinates(ρ, φ, εx, εy, _ => ρ * 20)
    })
  }
}