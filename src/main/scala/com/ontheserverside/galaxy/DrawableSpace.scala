package com.ontheserverside.galaxy

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import strawman.collection.IndexedView

object DrawableSpace {
  implicit def augmentSpace(space: Space): DrawableSpace = {
    new DrawableSpace(space)
  }
}

final class DrawableSpace(space: Space) {

  type Coordinates = IndexedView[(Double, Double)]

  private[this] def pointCartesianCoordinates: Coordinates = {
    space.points.view.map { point =>
      (point.position.x, point.position.y)
    }
  }

  private[this] def translate(coordinates: Coordinates, rMax: Double): Coordinates = {
    coordinates.map { case (x, y) =>
      (x + rMax, y + rMax)
    }
  }

  private[this] def scale(coordinates: Coordinates, width: Int, height: Int, rMax: Double): Coordinates = {
    val scaleX = width.toDouble / (2 * rMax)
    val scaleY = height.toDouble / (2 * rMax)

    val scale = if (scaleX < scaleY) scaleX else scaleY

    coordinates.map { case (x, y) =>
      (x * scale, y * scale)
    }
  }

  private[this] def crop(coordinates: Coordinates, width: Int, height: Int): Coordinates = {
    coordinates.filter { case (x,y) =>
      x > 0 && y > 0 && x < width && y < width
    }
  }

  /*
    Performs affine transformation of all `Space` points making them all be expressed by
    positive integers and fitting inside bounding box with specified `width` and `height`.
   */
  private[this] def transformedCoordinates(width: Int, height: Int, rMax: Double): Coordinates = {
    val translated = translate(pointCartesianCoordinates, rMax)
    val scaled = scale(translated, width, height, rMax)
    crop(scaled, width, height)
  }


  def draw(outputFile: File, width: Int, height: Int, rMax: Double): Unit = {
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

    val background = image.createGraphics();
    background.setPaint(Color.WHITE);
    background.fillRect(0, 0, image.getWidth, image.getHeight);

    transformedCoordinates(width, height, rMax).foreach { case (x, y) =>
      image.setRGB(x.toInt, y.toInt, Color.BLACK.getRGB)
    }

    ImageIO.write(image, "png", outputFile)

    image.flush()
  }
}