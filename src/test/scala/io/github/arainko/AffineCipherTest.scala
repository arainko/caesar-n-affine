package io.github.arainko

import io.github.arainko.cipher.Cipher
import io.github.arainko.model.common._
import zio.test.Assertion._
import zio.test._

object AffineCipherTest extends DefaultRunnableSpec {

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
        val crypto        = Crypto("HGAH")
        val expectedPlain = Plain("TEST")
        val bruteforced   = Cipher.affine.crack(crypto)
        assert(bruteforced)(hasSize(equalTo(312))) &&
        assert(bruteforced)(contains(expectedPlain))
      },
      test("decode with extra") {
        val crypto        = Crypto("HGAH")
        val extra         = Extra("TE")
        val expectedPlain = Plain("TEST")
        val expectedKey   = Key.Affine.Validated(4, 7)
        val result        = Cipher.affine.crackWithExtra(crypto, extra)
        assert(result)(isRight(equalTo(expectedKey -> expectedPlain)))
      }
    )
}
