object ByteUtils {

  val b0 = 0.toByte
  val b1 = 1.toByte
  val b2 = 2.toByte
  val b4 = 4.toByte
  val b8 = 8.toByte
  val b16 = 16.toByte
  val b32 = 32.toByte
  val b64 = 64.toByte
  val b128 = 128.toByte

  val bArray = Array(b128, b64, b32, b16, b8, b4, b2, b1)

  def toBits(byte: Byte): Array[Boolean] = bArray.map(b => (b & byte) == b)

  def toBits(bytes: Array[Byte]): Array[Boolean] = bytes.map(toBits).reduce(_ ++ _)

  def toBytes(bits: Array[Boolean]): Array[Byte] =
    safeWindows(bits, 8).map { arr =>
      arr.zip(bArray).filter(_._1).map(_._2).reduce((a, b) => (a ^ b).toByte)
    }

  //Might not be needed
  def safeSplit(bits: Array[Boolean], n: Int): (Array[Boolean], Array[Boolean]) =
    if (bits.size < n)
      (bits.padTo(n, false), Array.emptyBooleanArray)
    else
      bits.splitAt(n)

  def safeWindows(bits: Array[Boolean], size: Int): Array[Array[Boolean]] = {
    val windows = bits.sliding(size, size).toArray
    //Pad last one
    windows.update(windows.size - 1, windows.last.padTo(size, false))
    windows
  }

  //Not useful
  def pairBytes(bytes: Array[Byte]): Array[(Byte, Byte)] = {
    val pairedAlmost =
      bytes.foldLeft(
        (Array.empty[(Byte, Byte)], Option.empty[Byte])
      ) { (acc, byte) =>
        val (list, carry) = acc
        if (carry.isEmpty)
          (list, Some(byte))
        else
          (list ++ Array((carry.get, byte)), None)
      }
    if (pairedAlmost._2.isEmpty)
      pairedAlmost._1
    else
      pairedAlmost._1 ++ Array((pairedAlmost._2.get, 0.toByte))
  }

}
