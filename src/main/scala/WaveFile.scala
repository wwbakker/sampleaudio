import better.files._

import java.nio.{ByteBuffer, ByteOrder}

object WaveFile {
  private val RIFF_HEADER = Array[Byte]( 0x52, 0x49, 0x46, 0x46 )
  private val FORMAT_WAVE = Array[Byte](0x57, 0x41, 0x56, 0x45)
  private val FORMAT_TAG = Array[Byte](0x66, 0x6D, 0x74, 0x20)
  private val AUDIO_FORMAT = Array[Byte](0x1, 0x0)
  private val SUBCHUNK_ID = Array[Byte](0x64, 0x61, 0x74, 0x61)
  private val BYTES_PER_SAMPLE : Short = 1
  private val sampleRate = 48000
  private val channelCount : Short = 1
  private val stretch = 20
  private val byteRate: Int = sampleRate * channelCount * BYTES_PER_SAMPLE
  private val blockAlign: Short = (channelCount * BYTES_PER_SAMPLE).toShort


  def write(values: Seq[Int]): Unit = {
    val datalength: Int = values.length * stretch * BYTES_PER_SAMPLE
    val bb = ByteBuffer.allocate(datalength + 40)
    // ChunkID (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(RIFF_HEADER)
    // ChunkSize (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(datalength + 40)
    // Format (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(FORMAT_WAVE)
    // -- fmt subchunk
    // Subchunk1ID (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(FORMAT_TAG)
    // Subchunk1Size (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(16)
    // AudioFormat (2LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.put(AUDIO_FORMAT)
    // NumChannels (2LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putShort(channelCount)
    // SampleRate (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(sampleRate)
    // ByteRate (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(byteRate)
    // BlockAlign (2LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putShort(blockAlign)
    // BitsPerSample (2LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putShort((BYTES_PER_SAMPLE * 8).toShort)
    // /ExtraParamSize (2) PCM doesn't exist
    // /ExtraParams (?)
    // -- data subchunk
    // Subchunk2ID (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(SUBCHUNK_ID)
    // Subchunk2Size (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(datalength)
    // Data
    bb.order(ByteOrder.BIG_ENDIAN)
    val vMin = values.min.toDouble
    val vMax = values.max.toDouble
    val scale = vMax - vMin
    val normalizedValues = values.map( vOriginal => (vOriginal - vMin) / scale * 255).map(_.toInt)
    val stretched =
      interpolate(normalizedValues, stretch)
      .map(_.toByte).toArray

    bb.put(stretched)

    val wf = file"output.wav"
    if (wf.exists) {
      wf.delete()
    }
    wf.createFile()
      .appendByteArray(bb.array())
  }

  def interpolate(values : Seq[Int], times : Int) : Iterator[Double] =
    values
      .sliding(2)
      .map(_.toList)
      .flatMap{
        case begin :: end :: Nil =>
          val step = (end - begin) / times.toDouble
          Range(0, times).map(_ * step + begin)
      } ++ Iterator(values.last.toDouble)
}
