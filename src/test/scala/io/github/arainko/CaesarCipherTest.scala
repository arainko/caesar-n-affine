package io.github.arainko

import io.github.arainko.cipher.Cipher
import io.github.arainko.model.common._
import zio.test.Assertion._
import zio.test._

object CaesarCipherTest extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("Caesar cipher should")(
      test("encode") {
        val key      = Key.Caesar.Validated(1)
        val plain    = Plain("zZc! abc? abc-abcąęłżź")
        val encoded  = Cipher.caesar.encode(plain, key)
        val expected = Crypto("aAd! bcd? bcd-bcd")
        assert(encoded)(equalTo(expected))
      },
      test("decode") {
        val key             = Key.Caesar.Validated(1)
        val plain           = Plain("zZc! abc? abc-abcąęłżź")
        val encoded         = Cipher.caesar.encode(plain, key)
        val expectedEncoded = Crypto("aAd! bcd? bcd-bcd")
        val decoded         = Cipher.caesar.decode(expectedEncoded, key)
        val expectedDecoded = Plain("zZc! abc? abc-abc")
        assert(encoded)(equalTo(expectedEncoded)) &&
        assert(decoded)(equalTo(expectedDecoded))
      },
      test("brute force the cipher") {
        val key     = Key.Caesar.Validated(24)
        val plain   = Plain("zZc! abc? abc-abc")
        val encoded = Cipher.caesar.encode(plain, key)
        val cracked = Cipher.caesar.crack(encoded)
        assert(cracked)(contains(plain))
      },
      test("crack the cipher with extra text") {
        val key     = Key.Caesar.Validated(1)
        val plain   = Plain("zZc! abc? abc-abc")
        val encoded = Cipher.caesar.encode(plain, key)
        val extra   = Extra("zZc! a a a a a a a a a a a a ")
        val crackedWithExtra = Cipher.caesar
          .crackWithExtra(encoded, extra)
          .map { case (key, plain) => key.offset -> plain }
        assert(crackedWithExtra)(isRight(equalTo(key.offset -> plain)))
      }
    )
}
