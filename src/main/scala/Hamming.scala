case class HammingEncoded(bits: Array[Boolean]) {
  override def toString(): String = ByteUtils.toBytes(bits).map("%02x".format(_)).mkString("")
}

//big-endian
object Hamming {
  implicit val handler = HammingHandler(4)

  def main(args: Array[String]): Unit = {
    println("Welcome to Hamming!")
    val str = "Hello"
    val encoded = encode(str)
    println(s"$str becomes $encoded with size: ${encoded.bits.size} bits")
  }

  def encode(input: String): HammingEncoded = {
    val bits = ByteUtils.toBits(input.map(_.toByte).toArray)
    val bitWindows = ByteUtils.safeWindows(bits, handler.dataPosArr.size)

    HammingEncoded(bitWindows.map(HammingUtils.createHammingBlock).reduce(_ ++ _))
  }

  def clean(): Unit = {}

  def decode(input: HammingEncoded): String = ???
}
