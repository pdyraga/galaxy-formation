package com.ontheserverside.galaxy

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object DrawableSpace {
  implicit def augmentSpace(space: Space): DrawableSpace = {
    new DrawableSpace(space)
  }
}

/**
  Lets to draw Space instance to PNG file. You can later create animation with:

  ffmpeg -r 10 -f image2 -s 2000x2000 -i space-%05d.png -vcodec libx264 -crf 25 -pix_fmt yuv420p test.mp4
 */
class DrawableSpace(space: Space) {
  type Points = Iterable[(Double, Double)]

  /**
    * Reads and transforms cartesian coordinates of Space points.
    * We need to scale Space according to the provided factor as well as
    * translate Space position to put point (0, 0) in the center of image.
    * Last but not least, we filter out Space points no longer fitting image size.
    */
  private[this] def evaluateCoordinates(imageSize: Int, scale: Double): Points = {
    val translation = imageSize / 2

    space.points.view
      .map { point => (point.position.x, point.position.y) }
      .map { case (x,y) => (x * scale + translation, y * scale + translation) }
      .filter { case (x, y) => x > 0 && y > 0 && x < imageSize && y < imageSize }
  }

  def draw(outputFile: File, imageSize: Int, scale: Double): Unit = {
    val image = new BufferedImage(imageSize, imageSize, BufferedImage.TYPE_INT_ARGB);

    val background = image.createGraphics()
    background.setPaint(Color.BLACK)
    background.fillRect(0, 0, image.getWidth, image.getHeight)

    evaluateCoordinates(imageSize, scale).foreach { case (x, y) =>
      image.setRGB(x.toInt, y.toInt, Color.WHITE.getRGB)
    }

    ImageIO.write(image, "png", outputFile)

    image.flush()
  }
}
