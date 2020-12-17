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

  /**
   * Zip a list of data bits with their indicies
   *
   * @param data the data to zip
   * @return a list of pairs of data and their position
   */
  def zipWithData(data: Array[Boolean]): Array[(Boolean, Int)] = data.zip(dataPosArr)

  /**
   * Zip a list of parities bits with their indicies
   *
   * @param parities the parities to zip
   * @return a list of pairs of parities and their position
   */
  def zipWithParity(parities: Array[Boolean]): Array[(Boolean, Int)] = parities.zip(parityPosArr)
}

/**
 * An exception for bits which cannot be parsed by Hamming due to two errors
 *
 * @param bits the bits with two errors
 */
case class HammingException(bits: Array[Boolean]) extends RuntimeException

object HammingUtils {

  /**
   * Find all the trues in the given array
   *
   * @param arr an array of position zipped values to search
   * @return an array of positions of the trues
   */
  def findTrues(arr: Array[(Boolean, Int)]): Array[Int] = arr.filter(_._1).map(_._2)

  /**
   * Cound the number of trues in the given array
   *
   * @param arr an array of position zipped values to search
   * @return the number of true values
   */
  def countTrues(arr: Array[(Boolean, Int)]): Int = arr.filter(_._1).size

  /**
   * Get the parity values with their locations
   *
   * @param net the net parity value
   * @param handler the HammingHandler that has the positions
   * @return the pairs of parity values with their positions
   */
  def findParities(net: Int)(implicit handler: HammingHandler): Array[(Boolean, Int)] =
    handler.parityPosArr.map(i => ((i & net) == i, i))

  /**
   * Create a block of Hamming encoded data from the given data
   *
   * @param data the data to encode. should be of appropriate size for the handler
   * @param handler the HammingHandler to use
   * @return a block of Hamming encoded data
   */
  def createHammingBlock(data: Array[Boolean])(implicit handler: HammingHandler): Array[Boolean] = {
    val zipped = handler.zipWithData(data)
    val trues = findTrues(zipped)
    val netTrue = trues.fold(0)(_ ^ _)
    val parities = findParities(netTrue)
    val metaParity = ((countTrues(parities) + trues.length) % 2 == 1, handler.fullCheckPos)

    (Array(metaParity) ++ parities ++ zipped).sortBy(_._2).map(_._1)
  }

  /**
   * Get the data from the Hamming block
   *
   * @param data Hamming encoded data
   * @param handler the HammingHandler that can unpack the data
   * @return the ordered data from the block
   */
  def unpackHammingBlock(data: Array[Boolean])(implicit handler: HammingHandler): Array[Boolean] =
    handler.dataPosArr.map(data.apply)

  /**
   * Validate whether the given Hasmming Block can be safely unpacked by checking parities
   *
   * @param data the Hamming block to validate
   * @param handler the Handler for the HammingBlock
   * @return Either an error is uncorrectable (> 1 error) or the data possibly corrected for an single error
   */
  def validateParities(
    data: Array[Boolean]
  )(implicit handler: HammingHandler): Either[HammingException, Array[Boolean]] = {
    val zipped = data.zipWithIndex
    val trues = findTrues(zipped)
    val netTrue = trues.fold(0)(_ ^ _)

    val paritiesPass = netTrue == 0
    val metaPass = trues.length % 2 == 0
    println(s"netTrue: $netTrue, truesCount: ${trues.length}")

    //No error
    if (paritiesPass)
      Right(data)
    //Some error, but meta says it's 1 error
    else if (!metaPass) {
      val newData = data.clone()
      newData.update(netTrue, !data(netTrue))
      Right(newData)
    }
    else
      Left(HammingException(data))
  }

}
