package com.ontheserverside.galaxy.profile

import java.util.concurrent.ThreadLocalRandom

import com.ontheserverside.galaxy.Constants.yearsToSeconds
import com.ontheserverside.galaxy.{Point, Space}

object CartwheelProfile extends Profile {

  val stepDuration = yearsToSeconds(100)

  val softeningLength = 0.5e-5

  override def generateSpace: Space = {
    val pointsCount = 10000

    new Space(Array.fill(pointsCount) {
      val random = ThreadLocalRandom.current()

      val ρ = random.nextDouble(0, 0.25)
      val φ = random.nextDouble(0, Math.PI * 2)

      Point.fromPolarCoordinates(ρ, φ, 5 * ρ)
    })
  }
}