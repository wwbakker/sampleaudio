import better.files._

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object Program extends App {
  val pictureFile = file"io/cropped.png"
  val img = ImageIO.read(pictureFile.toJava)

  def values(image : BufferedImage): List[(Int, Int)] = {
    val result = collection.mutable.Buffer[(Int, Int)]()
    val raster = image.getData
    val sampleModel = image.getSampleModel
    val pixel = (x: Int, y: Int) => sampleModel.getPixel(x, y, null.asInstanceOf[Array[Int]], raster.getDataBuffer)
    val isPixelBright : (Int, Int) => Boolean = (x : Int, y : Int) => pixel(x, y)(1) > 127
    val height = image.getHeight

    Range(0, image.getWidth).foreach { x =>
      var max = 0
      var min = image.getHeight

      Range(0, image.getHeight).foreach { y =>
        if (isPixelBright(x, y)) {
          min = Math.min(y, min)
          max = Math.max(max, y)
        }
      }
      val pair = if (min == height && max == 0) (height / 2, height / 2) else (min, max)
      result += pair
    }
    result.toList
  }

  def averageOut(v : Seq[(Int, Int)]) : Seq[Int] =
    v.map{ case (a, b) => a + b / 2}

  def nonAverageOut(v : Seq[(Int, Int)]) : Seq[Int] =
    v.flatMap{ case (a, b) => Seq(a, b)}

  val samples = averageOut(values(img))

//  samples.foreach(println)
  WaveFile.write(samples)
  // https://www.youtube.com/watch?v=VQOdmckqNro
}
