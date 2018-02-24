package com.ontheserverside.galaxy.profile

import com.ontheserverside.galaxy.Space

trait Profile {
  def generateSpace: Space

  def stepDuration: Double

  def softeningLength: Double
}