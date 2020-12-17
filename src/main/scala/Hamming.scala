case class HammingEncoded(bits: Array[Boolean]) {
  override def toString(): String = ByteUtils.toBytes(bits).map("%02x".format(_)).mkString("")
}

object HammingEncoded {
  def fromString(str: String): HammingEncoded = ???
}

//big-endian chars
object Hamming {
  implicit val handler = HammingHandler(4)

  def main(args: Array[String]): Unit = {
    println("Welcome to Hamming!")
    val str = "Hello"
    val encoded = encode(str)
    println(s"$str becomes '0x$encoded' with size ${encoded.bits.size}")
    decode(encoded) match {
      case Left(err) => println(s"Could not decode: ${ByteUtils.toBytes(err.bits).map("%02x".format(_)).mkString("")} ${err.bits.mkString(", ")}")
      case Right(str) => println(s"Decoded: $str")
    }
    
  }

  def encode(input: String): HammingEncoded = {
    val bits = ByteUtils.toBits(input.map(_.toByte).toArray)
    val bitWindows = ByteUtils.safeWindows(bits, handler.dataPosArr.size)

    HammingEncoded(bitWindows.map(HammingUtils.createHammingBlock).reduce(_ ++ _))
  }

  def clean(): Unit = {}

  def decode(input: HammingEncoded): Either[HammingException, String] = 
    ByteUtils
      .safeWindows(input.bits, handler.fullSize)
      .foldLeft[Either[HammingException, Array[Boolean]]](Right(Array.emptyBooleanArray))((acc, bits) => {
        for(accArr <- acc.right; 
            parityResult <- HammingUtils.validateParities(bits).right) yield {
              accArr ++ HammingUtils.unpackHammingBlock(parityResult)
            }
      }).right.map(bits => ByteUtils.toBytes(bits).map(_.toChar).mkString(""))

}
