
/**
 * Handler for auto-generating size parity and data positions
 *
 * @param size power of 2 for which to create Hamming position values.
 * Example: size = 4 will create the psoition data for a block of 16 with a full parity check
 */
case class HammingHandler(size: Int) {
  lazy val parityPosArr: Array[Int] = (0 until size).map(Math.pow(2, _).toInt).toArray

  lazy val dataPosArr: Array[Int] =
    (3 until Math.pow(2, size).toInt).filter(i => !parityPosArr.contains(i)).toArray

  val fullCheckPos: Int = 0

  def zipWithData(data: Array[Boolean]): Array[(Boolean, Int)] = data.zip(dataPosArr)
  def zipWithParity(parities: Array[Boolean]): Array[(Boolean, Int)] = parities.zip(parityPosArr)
}


object HammingUtils {

  def findTrues(arr: Array[(Boolean, Int)]) : Array[Int] = arr.filter(_._1).map(_._2)

  def findParities(net: Int)(implicit handler: HammingHandler) : Array[(Boolean, Int)] = handler.parityPosArr.map(i => ((i & net) == i, i))

  def createHammingBlock(data: Array[Boolean])(implicit handler: HammingHandler) : Array[Boolean] = {
      val zipped = handler.zipWithData(data)
      val trues = findTrues(zipped)
      val netTrue = trues.reduce(_ ^ _)
      val parities = findParities(netTrue)
      val netParity = ((trues.size + findTrues(parities).size) % 2 == 1, handler.fullCheckPos)

      (Array(netParity) ++ parities ++ zipped).sortBy(_._2).map(_._1)
  }
}
