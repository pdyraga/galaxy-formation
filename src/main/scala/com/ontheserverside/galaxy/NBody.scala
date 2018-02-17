package com.ontheserverside.galaxy

import java.time.LocalDateTime

import com.google.common.util.concurrent.AtomicDoubleArray
import com.ontheserverside.galaxy.Constants.G

import scala.annotation.tailrec

class NBody(
  val totalSteps: Int,
  val stepTimespan: Double,
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

        // [F] = [pc/M_sun * (km/s)^2 * M_sun^2 * 1/pc^3 * pc] = [ M_sun * km/s^2 * (km/pc) ]
        // in order to eliminate (km/pc) piece, we need to multiply by Constants.kmPcRatio
        val forceScalar = (G * space.points(i).mass * space.points(j).mass / Math.pow(distanceVector.magnitude, 3.0)) * Constants.kmPcRatio

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
        x = point.position.x + stepTimespan * Constants.kmToPc(point.velocity.x),
        y = point.position.y + stepTimespan * Constants.kmToPc(point.velocity.y)
      )

      val newVelocity = point.velocity.copy(
        x = point.velocity.x + stepTimespan * forceFactorsX.get(pointIdx) / point.mass,
        y = point.velocity.y + stepTimespan * forceFactorsY.get(pointIdx) / point.mass
      )

      point.copy(position = newPosition, velocity = newVelocity)
    }.toArray)
  }
}