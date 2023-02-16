/**
 * Utility object for dealing with bytes and bits
 */
object ByteUtils {

  val byteSize = 8

  val b0 = 0.toByte
  val b1 = 1.toByte
  val b2 = 2.toByte
  val b4 = 4.toByte
  val b8 = 8.toByte
  val b16 = 16.toByte
  val b32 = 32.toByte
  val b64 = 64.toByte
  val b128 = 128.toByte

  /**
   * Big-endian array of 8 bits
   */
  val bArray = Array(b128, b64, b32, b16, b8, b4, b2, b1)

  /**
   * Transforms an byte into an big-endian list of 8 boolean bits
   *
   * @param byte the byte to transform
   * @return the list of big endian bits representing the byte
   */
  def toBits(byte: Byte): Array[Boolean] = bArray.map(b => (b & byte) == b)

  /**
   * Transforms a list of bytes in a big-endian list of boolean bits in corresponding order
   *
   * @param bytes the bytes to transform
   * @return a list big endian bits representing the bytes
   */
  def toBits(bytes: Array[Byte]): Array[Boolean] =
    bytes.map(toBits).fold(Array.emptyBooleanArray)(_ ++ _)

  def toByte(bits: Array[Boolean]): Byte = {
    bits.take(byteSize).zip(bArray)
        .filter(_._1)
        .map(_._2)
        .fold(0.toByte)((a, b) => (a ^ b).toByte)
  }

  /**
   * Transform the list of big-endian boolean bits into a list of bytes
   *
   * @param bits the bits into interpret
   * @return a list of bytes
   */
  def toBytes(bits: Array[Boolean]): Array[Byte] =
    safeWindows(bits, byteSize).map { arr =>
      arr
        .zip(bArray)
        .filter(_._1)
        .map(_._2)
        .fold(0.toByte)((a, b) => (a ^ b).toByte)
    }

  /**
   * Split the given array into windows of size n with no overlaps.
   * Every array returned should have the same size with the last array being padded with falses
   *
   * @param bits the array to split
   * @param size the size of each window
   * @return
   */
  def safeWindows(bits: Array[Boolean], size: Int): Array[Array[Boolean]] = {
    if (bits.isEmpty) Array.empty
    else {
      val windows = bits.sliding(size, size).toArray
      //Pad last one in place
      windows.update(windows.length - 1, windows.last.padTo(size, false))
      windows
    }
  }

  /**
   * Split the given array into two pieces: a head of the first n bits and a tail of the rest.
   * The operation is preformed safely, ensuring that the head is padded with falses if it would be too small.
   * The tail can be empty.
   *
   * N.B. Unused by the rest of this project
   *
   * @param bits the array to split
   * @param n the size to split off
   * @return a pair of the head array of size n and the tail of the rest of bits
   */
  def safeSplit(bits: Array[Boolean], n: Int): (Array[Boolean], Array[Boolean]) =
    if (bits.size < n)
      (bits.padTo(n, false), Array.emptyBooleanArray)
    else
      bits.splitAt(n)

  /**
   * Pair up a list of bytes. This is a manual window of 2.
   * If there is an odd number of bytes, the last byte is paired with a 0 byte.
   *
   * N.B. Unused by the rest of this project
   *
   * @param bytes the bytes to pair up
   * @return array of paired bytes
   */
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
