import scala.collection.mutable

case class HammingEncoded(bits: Array[Boolean]) {
  override def toString(): String = ByteUtils.toBytes(bits).map("%x".format(_)).mkString("")
}

object HammingEncoded {
  def fromOctal(str: String): HammingEncoded = {
    val builder = mutable.ArrayBuilder.make[Byte]()
    val lastCarry = str.map(c => {
      //Get integer value
      val ints = '0'.to('9').zipWithIndex
      val chars = 'a'.to('f').zipWithIndex.map(e => (e._1, e._2 + 10))
      ints.find(e => c.equals(e._1))
        .orElse(chars.find(e => c.equals(e._1)))
        .get._2
    })
      //Pair 2 to fit into Byte
      .foldLeft(Option.empty[Int])((carry, curr) => {
      if (carry.nonEmpty) {
        builder.+=((carry.get * 16 + curr).toByte)
        None
      }
      else Some(curr)
    })
    if (lastCarry.nonEmpty) builder.+=((lastCarry.get * 16).toByte)

    HammingEncoded(ByteUtils.toBits(builder.result()))

  }
}

trait ResultByte {
  val print : String
}

case class ResultChar(byte: Byte) extends ResultByte {
  val print: String = byte.toChar.toString()
}

case class ResultErr(byte: Byte) extends ResultByte {
  val print: String = "{error}"
}

//big-endian chars
object Hamming {
  implicit val handler = HammingHandler(4)

  def main(args: Array[String]): Unit = {
    args.head match {
      case "encode" => {
        val str = args.tail.mkString(" ")
        val encoded = encode(str)
        println(s"$str (len ${str.length}) \nbecomes \n'0x$encoded' (len ${encoded.bits.length/4})")
      }
      case "decode" => {
        val in = args.tail.head
        val clean = if(in.startsWith("0x")) in.drop(2) else in
        println(s"Decoded $clean to \n" + decodeWithPartials(HammingEncoded.fromOctal(clean)).map(_.print).mkString(""))
      }
    }


  }

  def encode(input: String): HammingEncoded = {
    val bits = ByteUtils.toBits(input.map(_.toByte).toArray)
    val bitWindows = ByteUtils.safeWindows(bits, handler.dataPosArr.size)

    HammingEncoded(bitWindows.map(HammingUtils.createHammingBlock).reduce(_ ++ _))
  }

  private def decodeOneShotBase(validator: (Array[Boolean]) => Either[HammingException, Array[Boolean]],
                                input: HammingEncoded): Either[HammingException, String] =
    ByteUtils
      .safeWindows(input.bits, handler.fullSize)
      .foldLeft[Either[HammingException, Array[Boolean]]](Right(Array.emptyBooleanArray))((acc, bits) => {
        for (accArr <- acc.right;
             parityResult <- validator(bits).right) yield
          accArr ++ HammingUtils.unpackHammingBlock(parityResult)

      }).right.map(bits => ByteUtils.toBytes(bits).map(_.toChar).mkString(""))

  def decodeOneShot(input: HammingEncoded): Either[HammingException, String] = decodeOneShotBase(HammingUtils.validateParities, input)
  def decodeOneShotNoMeta(input: HammingEncoded): Either[HammingException, String] = decodeOneShotBase(HammingUtils.validateParitiesNoMeta, input)

  private def decodeWithPartialsBase(validator: (Array[Boolean]) => Either[HammingException, Array[Boolean]],
                                     input: HammingEncoded): Array[ResultByte] = {
    val (main, extra, endErr) = ByteUtils
      .safeWindows(input.bits, handler.fullSize)
      .map(validator)
      .foldLeft((Array.empty[ResultByte], Array.emptyBooleanArray, false))((acc, currBlock) => {
        val (resultArr, carry, priorErr) = acc
        val (unpacked, currErr) = currBlock match {
          //Unpacked bits are untrustworthy
          case Left(err) => (HammingUtils.unpackHammingBlock(err.bits), true)
          //Unpacked bits are trustworthy
          case Right(block) => (HammingUtils.unpackHammingBlock(block), false)
        }
        val netBits = carry ++ unpacked

        //Take whole byte chunks
        val results = ByteUtils.toBytes(netBits.take(netBits.length / ByteUtils.byteSize * ByteUtils.byteSize))
          .zipWithIndex.map({case (b, idx) =>
          //Only first byte could have prior err effected data
          if (idx == 0 && priorErr) ResultErr(b)
          //All bytes are effected by current error
          else if (currErr) ResultErr(b)
          //Otherwise valid
          else ResultChar(b)
        })
        val tail = netBits.drop(results.length * ByteUtils.byteSize)

        //No data is no error
        val err = if (tail.isEmpty) false
        //Either new or carry could have produced error
        else if (netBits.length < ByteUtils.byteSize) priorErr || currErr
        //Only trailing chunk could have error
        else currErr
        (resultArr ++ results, tail, err)
      })
    main ++ ByteUtils.toBytes(extra).map(byte => if (endErr) ResultErr(byte) else ResultChar(byte))
  }
  def decodeWithPartials(input: HammingEncoded) : Array[ResultByte] = decodeWithPartialsBase(HammingUtils.validateParities, input)
  def decodeWithPartialsNoMeta(input: HammingEncoded) : Array[ResultByte] = decodeWithPartialsBase(HammingUtils.validateParitiesNoMeta, input)
}
