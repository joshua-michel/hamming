import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should._

class ByteUtilsSpec extends AnyFlatSpec with Matchers {
  val b0 = 0.toByte
  val b1 = 1.toByte

  "pairBytes" should "pair up bytes" in {
    ByteUtils.pairBytes(Array(1, 2, 3, 4).map(_.toByte)) shouldBe Array(
      (1.toByte, 2.toByte),
      (3.toByte, 4.toByte)
    )
  }

  it should "fill end with a 0" in {
    ByteUtils.pairBytes(Array(1, 2, 3).map(_.toByte)) shouldBe Array(
      (1.toByte, 2.toByte),
      (3.toByte, 0.toByte)
    )
  }

  "toBits" should "transform a byte into an array of bits" in {
    ByteUtils.toBits(7.toByte) shouldBe Array(false, false, false, false, false, true, true, true)
    ByteUtils.toBits(25.toByte) shouldBe Array(false, false, false, true, true, false, false, true)
  }

  "toBits" should "transforms multiple bytes into an array of bits" in {
    ByteUtils.toBits(Array(7.toByte, 25.toByte)) shouldBe
      Array(false, false, false, false, false, true, true, true, false, false, false, true, true,
        false, false, true)
  }

  "toBytes" should "transfrom array of bits into an array of bits" in {
      ByteUtils.toBytes(Array(false, false, false, false, false, true, true, true)) shouldBe Array(7.toByte)

      val arr = Array(7,25,91,10).map(_.toByte)

      ByteUtils.toBytes(ByteUtils.toBits(arr)) shouldBe arr
  }

  it should "pad with false if there is not enough data" in {
      ByteUtils.toBytes(Array(false, false, true, false, true, false)) shouldBe Array(40.toByte)
  }

  it should "return empty if provided no data" in {
    ByteUtils.toBytes(Array.empty) shouldBe empty
  }

  "safeSplit" should "split a boolean Array" in {
    val res = ByteUtils.safeSplit(Array(true, true, false, false), 2)
    res._1 shouldBe Array(true, true)
    res._2 shouldBe Array(false, false)
  }

  it should "fill with falses if array is too small" in {
    val res = ByteUtils.safeSplit(Array(true, true, false, false), 6)
    res._1 shouldBe Array(true, true, false, false, false, false)
    res._2 shouldBe empty
  }

  "safeWindows" should "window a boolean Array" in {
    val res = ByteUtils.safeWindows(
      Array(false, false, false, false, false, true, true, true, false, false, false, true, true,
        false, false, true),
      8
    )
    res.size shouldBe 2
    res.head shouldBe ByteUtils.toBits(7.toByte)
  }

  it should "pad the last array with falses if it too small" in {
    val res = ByteUtils.safeWindows(
      Array(false, false, false, false, false, true, true, true, false, false, false, true, true,
        false),
      8
    )
    res.size shouldBe 2
    res(1) shouldBe ByteUtils.toBits(24.toByte)
  }

  it should "return empty if provided empty" in {
    val res = ByteUtils.safeWindows(Array.empty, 8)
    res shouldBe empty
  }

}
