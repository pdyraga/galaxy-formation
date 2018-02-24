package com.ontheserverside.galaxy

import java.io.File
import DrawableSpace._

object Simulation {
  def main(args: Array[String]): Unit = {
    val totalNumberOfSteps = args.headOption.map(_.toInt).getOrElse(Int.MaxValue)
    execute(BulgeProfile, totalNumberOfSteps)
  }

  private[this] def execute(profile: Profile, totalSteps: Int): Unit = {
    val space = profile.generateSpace
    draw(space, 0)
    val softeningLength = 0.5e-5
    new NBody(totalSteps, profile.stepDuration, draw, softeningLength).execute(space)
  }

  private[this] def draw(space: Space, stepNumber: Int): Unit = {
    space.draw(
      outputFile = new File(f"/tmp/simulation/space-$stepNumber%010d.png"),
      imageSize = 2000,
      scale = 1000
    )
  }
}