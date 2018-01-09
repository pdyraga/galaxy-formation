package com.ontheserverside.galaxy

import java.io.File

import DrawableSpace._
import strawman.collection.immutable.ImmutableArray
import strawman.collection.mutable.ArrayBuffer

import scala.annotation.tailrec

class Simulation(
  val totalSteps: Int,
  val stepTimespan: Double,
  val rMax: Double,
  val onStepCompleted: (Space, Int) => Unit
) extends Constants {

  def execute(space: Space): Space = execute(space, 0)

  @tailrec
  private[this] def execute(space: Space, stepsSoFar: Int): Space = {
    println(s"Executing step $stepsSoFar out of $totalSteps")

    if (stepsSoFar == totalSteps) {
      space
    } else {
      val transformed = executeStep(space)
      onStepCompleted(transformed, stepsSoFar)
      execute(transformed, stepsSoFar + 1)
    }
  }

  private[this] def executeStep(space: Space): Space = {
    val forceFactors = ArrayBuffer.fill(space.points.length)((0.0, 0.0))

    space.points.view.zipWithIndex.foreach { case (point, pointIdx) =>
      val otherPointsToCheck = space.points
        .view
        .zipWithIndex
        .drop(pointIdx + 1)


      //TODO: parallelize this loop
      otherPointsToCheck.foreach { case (anotherPoint, anotherPointIdx) =>

        val distanceVector = point.distanceVector(anotherPoint)
        val forceScalar = G * anotherPoint.mass / Math.pow(distanceVector.magnitude, 3.0) //TODO: we can move G out of here
        val forceFactor = (
          distanceVector.headX * forceScalar,
          distanceVector.headY * forceScalar
        )

        forceFactors(pointIdx) = (
          forceFactors(pointIdx)._1 + forceFactor._1,
          forceFactors(pointIdx)._2 + forceFactor._2
        )

        forceFactors(anotherPointIdx) = (
          forceFactors(anotherPointIdx)._1 - forceFactor._1,
          forceFactors(anotherPointIdx)._2 - forceFactor._2
        )
      }
    }

    new Space(ImmutableArray.from(space.points.view.zipWithIndex.map { case (point, pointIdx) =>
      val newPosition = point.positionVector.copy(
        headX = point.positionVector.headX + stepTimespan * point.velocityVector.headX,
        headY = point.positionVector.headY + stepTimespan * point.velocityVector.headY
      )

      val newVelocity = point.velocityVector.copy(
        headX = point.velocityVector.headX + stepTimespan * forceFactors(pointIdx)._1,
        headY = point.velocityVector.headY + stepTimespan * forceFactors(pointIdx)._2
      )

      point.copy(positionVector = newPosition, velocityVector = newVelocity)
    }))
  }
}

object Simulation {
  // ffmpeg -r 10 -f image2 -s 600x600 -i space-%05d.png -vcodec libx264 -crf 25  -pix_fmt yuv420p test.mp4
  def main(args: Array[String]): Unit = {
    val rMax = 10

    val space = Space.generateHomogeneousSpace(1, rMax)

    val onStepCompleted = (s: Space, stepNumber: Int) => draw(s, f"space-$stepNumber%05d", rMax)
    new Simulation(2500, 0.1, rMax, onStepCompleted).execute(space)
  }

  //TODO: draw in a searate thread
  private[this] def draw(space: Space, fileName: String, rMax: Int) = {
    space.draw(new File(s"/tmp/simulation/$fileName.png"), 600, 600, rMax)
  }
}