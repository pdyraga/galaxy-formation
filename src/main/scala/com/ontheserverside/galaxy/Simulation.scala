package com.ontheserverside.galaxy

import java.io.File

import DrawableSpace._

object Simulation {
  def main(args: Array[String]): Unit = {
    val rMax = 1000
    val space = Space.generateHomogeneousSpace(1000000, rMax)
    space.draw(new File(s"/tmp/simulation/space.png"), 2000, 2000, rMax)
  }
}
