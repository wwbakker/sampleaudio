import better.files._

import java.nio.{ByteBuffer, ByteOrder}

object WaveFile {
  private val RIFF_HEADER = Array[Byte]( 0x52, 0x49, 0x46, 0x46 )
  private val FORMAT_WAVE = Array[Byte](0x57, 0x41, 0x56, 0x45)
  private val FORMAT_TAG = Array[Byte](0x66, 0x6D, 0x74, 0x20)
  private val AUDIO_FORMAT = Array[Byte](0x1, 0x0)
  private val SUBCHUNK_ID = Array[Byte](0x64, 0x61, 0x74, 0x61)
  private val HEADER_SIZE_BYTES = 44
  private val sampleRate = 48000
  private val channelCount : Short = 1


  def write(originalSamples: Seq[Int], durationInMs : Int, bytesPerSample: Int = 1): Unit = {
    val numberOfSamplesRequired = durationInMs * (sampleRate / 1000)
    val stretch = numberOfSamplesRequired / originalSamples.length
    val byteBuffer = writeWaveHeader(originalSamples.length * stretch, bytesPerSample)
    writeWaveBody(byteBuffer, originalSamples, bytesPerSample, stretch)


    val wf = file"output.wav"
    if (wf.exists) {
      wf.delete()
    }
    wf.createFile()
      .appendByteArray(byteBuffer.array())
  }
  private def writeWaveBody(byteBuffer: ByteBuffer,
                            originalSamples: Seq[Int],
                            bytesPerSample: Int,
                            stretchFactor: Int): Unit = {
    val vMin = originalSamples.min.toDouble
    val vMax = originalSamples.max.toDouble
    val originalSampleResolution = vMax - vMin
    val newSampleResolution = Math.pow(255, bytesPerSample)
    val scale = newSampleResolution / originalSampleResolution
    val normalizedValues = originalSamples.map(vOriginal => (vOriginal - vMin) * scale).map(_.toInt)
    val stretched = interpolate(normalizedValues, stretchFactor).toSeq

    // Data
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    bytesPerSample match {
      case 1 => stretched.foreach(sample => byteBuffer.put(sample.toByte))
      case 2 => stretched.foreach(sample => byteBuffer.putShort(sample.toShort))
    }
  }

  private def writeWaveHeader(numberOfSamples: Int, bytesPerSample : Int): ByteBuffer = {
    val dataLength: Int = numberOfSamples * bytesPerSample
    val bb = ByteBuffer.allocate(dataLength + HEADER_SIZE_BYTES)
    val byteRate: Int = sampleRate * channelCount * bytesPerSample
    val blockAlign: Short = (channelCount * bytesPerSample).toShort

    // ChunkID (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(RIFF_HEADER)
    // ChunkSize (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(dataLength + HEADER_SIZE_BYTES)
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
    bb.putShort((bytesPerSample * 8).toShort)
    // /ExtraParamSize (2) PCM doesn't exist
    // /ExtraParams (?)
    // -- data subchunk
    // Subchunk2ID (4BE)
    bb.order(ByteOrder.BIG_ENDIAN)
    bb.put(SUBCHUNK_ID)
    // Subchunk2Size (4LE)
    bb.order(ByteOrder.LITTLE_ENDIAN)
    bb.putInt(dataLength)
  }

  private def interpolate(values : Seq[Int], times : Int) : Iterator[Double] =
    (values :+ values.last)
      .sliding(2)
      .map(_.toList)
      .flatMap{
        case begin :: end :: Nil =>
          val step = (end - begin) / times.toDouble
          Range(0, times).map(_ * step + begin)
      }
}
