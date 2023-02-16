import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should._

class HammingSpec extends AnyFlatSpec with Matchers {

  "HammingEncoded" should "print as hex" in {
    HammingEncoded(Array(false, false, false, false, false, false, false, false)).toString() shouldBe "0"
    HammingEncoded(Array(false, false, false, false, false, true, false, false)).toString() shouldBe "4"
    HammingEncoded(Array(true, false, false, false, true, false, true, true)).toString() shouldBe "8b"
  }

  it should "read in hex" in {
    HammingEncoded.fromOctal("8b").toString() shouldBe "8b"
    HammingEncoded.fromOctal("1487bfac1390").toString() shouldBe "1487bfac1390"
  }

  it should "append 0s to the end of trailing bytes" in {
    HammingEncoded.fromOctal("50").toString() shouldBe "50"
  }

  "encode" should "encode data" in {
    Hamming.encode("t").toString() shouldBe "afa0"
  }

  "decodeOneShot" should "pass on perfect data" in {
    Hamming.decodeOneShot(HammingEncoded.fromOctal("afa0")).right.get.take(1) shouldBe "t"
    Hamming.decodeOneShot(HammingEncoded.fromOctal("6fa3ca5cbde8")).right.get.take(4) shouldBe "test"
  }

  it should "fail on 2 bit flips" in {
    //afa0 => `f` flips 1 and 2 bits to get to `c`
    Hamming.decodeOneShot(HammingEncoded.fromOctal("aca0")).left.get.bits shouldBe
      Seq(true, false, true, false, true, true, false, false,
        true, false, true, false, false, false, false, false)
  }

  it should "pass on 1 bit flips" in {
    //afa0 => `f` flips 1 bit to get to `e`
    Hamming.decodeOneShot(HammingEncoded.fromOctal("aea0")).right.get.take(1) shouldBe "t"
  }

  it should "pass on 1 bit flips in multiple blocks" in {
    Hamming.decodeOneShot(HammingEncoded.fromOctal("6ea3ca7c3de8")).right.get.take(4) shouldBe "test"
  }

  "decodeOneShotNoMeta" should "pass on perfect data" in {
    Hamming.decodeOneShotNoMeta(HammingEncoded.fromOctal("afa0")).right.get.take(1) shouldBe "t"
    Hamming.decodeOneShotNoMeta(HammingEncoded.fromOctal("6fa3ca5cbde8")).right.get.take(4) shouldBe "test"
  }

  it should "fail on 2 bit flips" in {
    //afa0 => `f` flips 1 and 2 bits to get to `c`
    Hamming.decodeOneShotNoMeta(HammingEncoded.fromOctal("aca0")).left.get.bits shouldBe
      Seq(true, false, true, false, true, true, false, false,
        true, false, true, false, false, false, false, false)
  }

  it should "fail on 1 bit flips" in {
    //afa0 => `f` flips 1 bit to get to `e`
    Hamming.decodeOneShotNoMeta(HammingEncoded.fromOctal("aea0")).left.get.bits shouldBe
      Seq(true, false, true, false, true, true, true, false,
        true, false, true, false, false, false, false, false)
  }

  it should "pass on 1 bit flips in multiple blocks" in {
    Hamming.decodeOneShotNoMeta(HammingEncoded.fromOctal("6ea3ca7c3de8")).isLeft shouldBe true
  }

  "decodeWithPartials" should "pass on perfect data" in {
    Hamming.decodeWithPartials(HammingEncoded.fromOctal("afa0")).take(1).map(_.print).mkString("") shouldBe "t"
    Hamming.decodeWithPartials(HammingEncoded.fromOctal("6fa3ca5cbde8")).take(4).map(_.print).mkString("") shouldBe "test"
  }

  it should "fail on 2 bit flips" in {
    //afa0 => `f` flips 1 and 2 bits to get to `c`
    val fail = Hamming.decodeWithPartials(HammingEncoded.fromOctal("aca0"))
    fail.length shouldBe 2
    fail.take(1).map(_.print).mkString shouldBe "{error}"
    fail.head shouldBe a[ResultErr]
  }

  it should "pass on 1 bit flips" in {
    //afa0 => `f` flips 1 bit to get to `e`
    Hamming.decodeWithPartials(HammingEncoded.fromOctal("aea0")).take(1).map(_.print).mkString("") shouldBe "t"
  }

  it should "pass on 1 bit flips in multiple blocks" in {
    Hamming.decodeWithPartials(HammingEncoded.fromOctal("6ea3ca7c3de8")).take(4).map(_.print).mkString("") shouldBe "test"
  }
    
}
