package io.github.arainko

import zio.test._
import zio.test.Assertion._
import io.github.arainko.model.common._

object CaesarCipherTest extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Caesar cipher should")(
      test("encode") {
        val key      = Key.Caesar(1)
        val plain    = Plain("zZc! abc? abc-abcąęłżź")
        val encoded  = Cipher.caesar.encode(plain, key)
        val expected = Crypto("aAd! bcd? bcd-bcd")
        assert(encoded)(equalTo(expected))
      }
    )
}
