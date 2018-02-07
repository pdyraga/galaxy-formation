package com.ontheserverside.galaxy

import java.io.File
import java.time.LocalDateTime

import com.google.common.util.concurrent.AtomicDoubleArray

import scala.annotation.tailrec

import DrawableSpace._
import Constants._

class Simulation(
  val totalSteps: Int,
  val stepTimespan: Double,
  val rMax: Double,
  val onStepCompleted: (Space, Int) => Unit
) {

  def execute(space: Space): Space = execute(space, 0)

  @tailrec
  private[this] def execute(space: Space, stepsSoFar: Int): Space = {
    println(s"[${LocalDateTime.now}] Executing step $stepsSoFar out of $totalSteps")

    if (stepsSoFar == totalSteps) {
      space
    } else {
      val transformed = executeStep(space)
      onStepCompleted(transformed, stepsSoFar)
      execute(transformed, stepsSoFar + 1)
    }
  }

  private[this] def executeStep(space: Space): Space = {
    val forceFactorsX = new AtomicDoubleArray(space.points.length)
    val forceFactorsY = new AtomicDoubleArray(space.points.length)

    for (i <- 0 until space.points.length) {
      for (j <- (i + 1 until space.points.length).par) {

        val distanceVector = space.points(i).distanceVector(space.points(j))
        val forceScalar = G * space.points(i).mass * space.points(j).mass / Math.pow(distanceVector.magnitude, 3.0)
        val forceFactor = (
          distanceVector.x * forceScalar,
          distanceVector.y * forceScalar
        )

        forceFactorsX.addAndGet(i, forceFactor._1)
        forceFactorsY.addAndGet(i, forceFactor._2)

        forceFactorsX.addAndGet(j, -1 * forceFactor._1)
        forceFactorsY.addAndGet(j, -1 * forceFactor._2)
      }
    }

    new Space(space.points.view.zipWithIndex.map { case (point, pointIdx) =>
      val newPosition = point.position.copy(
        x = point.position.x + stepTimespan * point.velocity.x,
        y = point.position.y + stepTimespan * point.velocity.y
      )

      val newVelocity = point.velocity.copy(
        x = point.velocity.x + stepTimespan * forceFactorsX.get(pointIdx) / point.mass,
        y = point.velocity.y + stepTimespan * forceFactorsY.get(pointIdx) / point.mass
      )

      point.copy(position = newPosition, velocity = newVelocity)
    }.toArray)
  }
}

object Simulation {
  // ffmpeg -r 10 -f image2 -s 2000x2000 -i space-%05d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test.mp4
  def main(args: Array[String]): Unit = {
    val rMax = 1000

    val space = Space.generateHomogeneousSpace(10000, rMax)

    val onStepCompleted = (s: Space, stepNumber: Int) => draw(s, f"space-$stepNumber%05d", rMax)
    new Simulation(1000, 60, rMax, onStepCompleted).execute(space)
  }

  private[this] def draw(space: Space, fileName: String, rMax: Double) = {
    space.draw(
      outputFile = new File(s"/tmp/simulation/$fileName.png"),
      imageSize = 2000,
      scale = 0.5
    )
  }
}