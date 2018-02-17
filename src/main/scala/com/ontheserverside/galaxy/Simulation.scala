package com.ontheserverside.galaxy

import java.io.File
import DrawableSpace._

object Simulation {
  // ffmpeg -r 10 -f image2 -s 2000x2000 -i space-%05d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test.mp4
  def main(args: Array[String]): Unit = {
    execute(
      profile = BulgeProfile,
      totalSteps = 100,
      onStepCompleted = (s: Space, stepNumber: Int) => draw(s, f"space-$stepNumber%05d")
    )
  }

  private[this] def execute(profile: Profile, totalSteps: Int, onStepCompleted: (Space, Int) => Unit): Unit = {
    val nBody = new NBody(totalSteps, profile.stepDuration, profile.rMax, onStepCompleted)
    nBody.execute(profile.generateSpace)
  }

  private[this] def draw(space: Space, fileName: String) = {
    space.draw(
      outputFile = new File(s"/tmp/simulation/$fileName.png"),
      imageSize = 2000,
      scale = 100
    )
  }
}