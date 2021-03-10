package io.github.arainko

import zio.test._
import zio.test.Assertion._
import io.github.arainko.model.common._

object AffineCipherTest extends DefaultRunnableSpec {

  def inverseInRing(ring: Int)(value: Int): Option[Int] =
    (0 until ring).find(elem => Math.floorMod(elem * value, ring) == 1)

  def spec: ZSpec[Environment, Failure] =
    suite("Affine Cipher test")(
      test("encode") {
        val plain          = Plain("TEST")
        val key            = Key.Affine.Validated(4, 7)
        val crypto         = Cipher.affine.encode(plain, key)
        val expectedCrypto = Crypto("HGAH")
        assert(crypto)(equalTo(expectedCrypto))
      },
      test("decode") {
        val crypto        = Crypto("HGAH")
        val key           = Key.Affine.Validated(4, 7)
        val plain         = Cipher.affine.decode(crypto, key)
        val expectedPlain = Plain("TEST")
        assert(plain)(equalTo(expectedPlain))
      },
      test("bruteforce") {
        val crypto = Crypto("HGAH")
        val expectedPlain = Plain("TEST")
        val bruteforced = Cipher.affine.crack(crypto)
        assert(bruteforced)(hasSize(equalTo(312))) &&
        assert(bruteforced)(contains(expectedPlain))
      }
    )
}
