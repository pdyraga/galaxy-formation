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

class DrawableSpace(space: Space) {
  type Points = IndexedView[(Double, Double)]

  private[this] def crop(points: Points, imageSize: Int): Points = {
    points.filter { case (x,y) =>
      x > 0 && y > 0 && x < imageSize && y < imageSize
    }
  }

  private[this] def affineTransformation(points: Points, imageSize: Int, scale: Double): Points = {
    val translation = imageSize / 2

    val scaledAndTranslated = points.map { case (x, y) =>
      (x * scale + translation, y * scale + translation)
    }

    crop(scaledAndTranslated, imageSize)
  }

  private[this] def pointCartesianCoordinates: Points = {
    space.points.view.map { point =>
      (point.position.x, point.position.y)
    }
  }

  def draw(outputFile: File, imageSize: Int, scale: Double): Unit = {
    val image = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_ARGB);

    val background = image.createGraphics();
    background.setPaint(Color.BLACK);
    background.fillRect(0, 0, image.getWidth, image.getHeight);

    affineTransformation(pointCartesianCoordinates, imageSize, scale).foreach { case (x, y) =>
      image.setRGB(x.toInt, y.toInt, Color.WHITE.getRGB)
    }

    ImageIO.write(image, "png", outputFile)

    image.flush()
  }
}
