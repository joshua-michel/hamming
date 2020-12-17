import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest._
import matchers.should._

class HammingUtilsSpec extends AnyFlatSpec with Matchers {

    trait HUCommons {
        val handler3 = HammingHandler(3)
        val data3 = Array(true, false, true, false)
        val dataIdx3 = data3.zipWithIndex
        val dataPos3 = handler3.zipWithData(data3)
        val pack3 = Array(false, true, false, true, true, false, true, false)

        val par3_6 = Array(false, true, true)
    }

    "HammingHandler" should "create correct parity positions" in {
        HammingHandler(4).parityPosArr shouldBe Array(1,2,4,8)
        HammingHandler(6).parityPosArr shouldBe Array(1,2,4,8,16,32)
    }

    it should "create correct data positions" in {
        HammingHandler(3).dataPosArr shouldBe Array(3,5,6,7)
        HammingHandler(4).dataPosArr shouldBe Array(3,5,6,7,9,10,11,12,13,14,15)
    }

    it should "have position count total be equal to 2 ^ size - 1" in {
        val handler = HammingHandler(4)
        handler.parityPosArr.size + handler.dataPosArr.size shouldBe 15
        val handler10 = HammingHandler(10)
        handler10.parityPosArr.size + handler10.dataPosArr.size shouldBe 1023
    }

    "findTrues" should "get the positions of the trues" in new HUCommons {
        HammingUtils.findTrues(dataIdx3) shouldBe Array(0,2)
        HammingUtils.findTrues(dataPos3) shouldBe Array(3,6)
    }

    "findParities" should "find the parities to cancel out the given value" in new HUCommons {
        HammingUtils.findParities(6)(handler3) should contain theSameElementsAs handler3.zipWithParity(par3_6)
    }

    "createHammingBlock" should "create a block of hamming encoded data" in new HUCommons {
        HammingUtils.createHammingBlock(data3)(handler3) shouldBe pack3
    }

    "unpackHammingBlock" should "get the data from a hamming block" in new HUCommons {
        HammingUtils.unpackHammingBlock(pack3)(handler3) shouldBe data3 
    }

    "validateParities" should "identify a correct Hamming Block" in new HUCommons {
        HammingUtils.validateParities(pack3)(handler3) shouldBe Right(pack3) 
    }
    
    it should "identify an error in the data" in new HUCommons {
        val res = HammingUtils.validateParities(Array(false, true, false, true, true, false, false, false))(handler3)
        res.isRight shouldBe true
        res.right.get shouldBe pack3
    }
    
    it should "identify an error in the parities" in new HUCommons {
        val res = HammingUtils.validateParities(Array(false, false, false, true, true, false, true, false))(handler3) 
        res.isRight shouldBe true
        res.right.get shouldBe pack3
    }
    
    it should "identify whether there are two errors" in new HUCommons {
        val res = HammingUtils.validateParities(Array(false, false, false, true, true, false, false, false))(handler3) 
        res.isLeft shouldBe true
        res.left.get shouldBe a [HammingException]
        res.left.get.bits shouldBe Array(false, false, false, true, true, false, false, false)
    }
}
