package com.ontheserverside.galaxy

import strawman.collection.immutable.ImmutableArray

case class Point(
  positionVector: PositionVector,
  velocityVector: EuclideanVector,
  mass: Double = 1.0
)

class Space(val points: ImmutableArray[Point])
