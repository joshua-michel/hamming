/**
 * Handler for auto-generating size parity and data positions
 *
 * @param size power of 2 for which to create Hamming position values.
 * Example: size = 4 will create the psoition data for a block of 16 with a full parity check
 */
case class HammingHandler(size: Int) {
  val fullSize: Int = Math.pow(2, size).toInt

  lazy val parityPosArr: Array[Int] = (0 until size).map(Math.pow(2, _).toInt).toArray

  lazy val dataPosArr: Array[Int] =
    (3 until fullSize).filter(i => !parityPosArr.contains(i)).toArray

  val fullCheckPos: Int = 0

  def zipWithData(data: Array[Boolean]): Array[(Boolean, Int)] = data.zip(dataPosArr)
  def zipWithParity(parities: Array[Boolean]): Array[(Boolean, Int)] = parities.zip(parityPosArr)
}

case class HammingException(bits: Array[Boolean]) extends RuntimeException

object HammingUtils {

  def findTrues(arr: Array[(Boolean, Int)]): Array[Int] = arr.filter(_._1).map(_._2)

  def countTrues(arr: Array[(Boolean, Int)]): Int = arr.filter(_._1).size

  def findParities(net: Int)(implicit handler: HammingHandler): Array[(Boolean, Int)] =
    handler.parityPosArr.map(i => ((i & net) == i, i))

  def createHammingBlock(data: Array[Boolean])(implicit handler: HammingHandler): Array[Boolean] = {
    val zipped = handler.zipWithData(data)
    val trues = findTrues(zipped)
    val netTrue = trues.fold(0)(_ ^ _)
    val parities = findParities(netTrue)
    val metaParity = (countTrues(parities) % 2 == 1, handler.fullCheckPos)

    (Array(metaParity) ++ parities ++ zipped).sortBy(_._2).map(_._1)
  }

  def unpackHammingBlock(data: Array[Boolean])(implicit handler: HammingHandler): Array[Boolean] =
    handler.dataPosArr.map(data.apply)

  def validateParities(
    data: Array[Boolean]
  )(implicit handler: HammingHandler): Either[HammingException, Int] = {
    val zipped = data.zipWithIndex
    val trues = findTrues(zipped)
    val netTrue = trues.fold(0)(_ ^ _)

    val paritiesPass = netTrue == 0
    val metaPass =
      (countTrues(handler.parityPosArr.map(i => (data(i), i))) % 2 == 1) == data(handler.fullCheckPos)

    if (paritiesPass && metaPass)
      Right(netTrue)
    else
      Left(HammingException(data))
  }

}
