package xiangshan

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlatformGrainTest extends AnyFlatSpec with Matchers {
  behavior of "PlatformGrain width safety and bounds"

  it should "compute correct shift and mask for large grains up to and beyond 32 bits" in {
    for (grain <- Seq(12, 30, 31, 32, 33, 40)) {
      val grainBytes = BigInt(1) << grain
      grainBytes should be > BigInt(0)
      grainBytes shouldBe (BigInt(1) << grain)

      val pmpOffBits = 2
      val matchMaskAddr = (((BigInt(1) << grain) - 1) >> pmpOffBits)
      matchMaskAddr should be > BigInt(0)
      matchMaskAddr.bitLength shouldBe (grain - pmpOffBits)
    }
  }

  it should "enforce valid bounds in PMParameters" in {
    // Valid grain
    PMParameters(PlatformGrain = 12).PlatformGrain shouldBe 12
    PMParameters(PlatformGrain = 32).PlatformGrain shouldBe 32

    // Out-of-bounds grains should fail validation
    an[IllegalArgumentException] should be thrownBy {
      PMParameters(PlatformGrain = 1)
    }
    an[IllegalArgumentException] should be thrownBy {
      PMParameters(PlatformGrain = 64)
    }
  }
}
