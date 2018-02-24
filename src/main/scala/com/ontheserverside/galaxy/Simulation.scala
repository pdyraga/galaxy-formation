package com.ontheserverside.galaxy

import java.io.File

import DrawableSpace._
import com.ontheserverside.galaxy.profile.{CartwheelProfile, Profile}

object Simulation {
  def main(args: Array[String]): Unit = {
    val totalNumberOfSteps = args.headOption.map(_.toInt).getOrElse(Int.MaxValue)
    execute(CartwheelProfile, totalNumberOfSteps)
  }

  private[this] def execute(profile: Profile, totalSteps: Int): Unit = {
    val space = profile.generateSpace
    draw(space, 0)
    new NBody(totalSteps, profile.stepDuration, draw, profile.softeningLength).execute(space)
  }

  private[this] def draw(space: Space, stepNumber: Int): Unit = {
    space.draw(
      outputFile = new File(f"/tmp/simulation/space-$stepNumber%010d.png"),
      imageSize = 2000,
      scale = 500
    )
  }
}
