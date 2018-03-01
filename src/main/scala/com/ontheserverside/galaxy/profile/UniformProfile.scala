package com.ontheserverside.galaxy.profile

import com.ontheserverside.galaxy.Constants.yearsToSeconds
import com.ontheserverside.galaxy.Space

object UniformProfile extends Profile with UniformPointDistributionSupport {

  val stepDuration = yearsToSeconds(100000)

  val softeningLength = 0.5e-5

  private[this] val pointsCount = 10000

  private[this] val rMax = 10

  override def generateSpace: Space = {
    new Space(Array.fill(pointsCount)(
      createPoint(0, rMax, _ => 0)
    ))
  }
}
