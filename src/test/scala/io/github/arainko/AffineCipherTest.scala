package io.github.arainko

import io.github.arainko.cipher.Cipher
import io.github.arainko.model.common._
import zio.test.Assertion._
import zio.test._

object AffineCipherTest extends DefaultRunnableSpec {

  private val lowerCaseAlphabet         = ('a' to 'z').toSet
  private val upperCaseAlphabet         = ('A' to 'Z').toSet
  private val alph = lowerCaseAlphabet ++ upperCaseAlphabet
  private val punctuationRegex          = "\\p{Punct}".r
  private def isPunctuation(char: Char) = punctuationRegex.matches(char.toString)

  private def limitString(c: Char) = alph.contains(c) || c.isWhitespace || isPunctuation(c) || c.isDigit
    
  val keyGen = for {
    offset <- Gen.int(1, 25)
    factor <- Gen.fromIterable(Set(1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25))
  } yield Key.Affine.Validated(offset, factor)

  val plainGen = Gen.anyASCIIString
    .map(_.filter(limitString))
    .map(Plain)

  val plainSizeBiggerThan2Gen =
    Gen.stringBounded(2, 100)(Gen.anyChar.filter(limitString)).map(Plain)
  
  // plainGen.filter(_.text.size >= 2)

  private val affinePropertySpec = suite("Affine Property Spec")(
    testM("should encode and decode back") {
      check(plainGen, keyGen) { (plain, key) =>
        val encoded = Cipher.affine.encode(plain, key)
        val decoded = Cipher.affine.decode(encoded, key)
        assert(decoded)(equalTo(plain))
      }
    },
    testM("should bruteforce and contain plain") {
      check(plainGen, keyGen) { (plain, key) =>
        val encoded = Cipher.affine.encode(plain, key)
        val bruteforced = Cipher.affine.crack(encoded)
        assert(bruteforced)(contains(plain))
      }
    }
  )

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
      },
      affinePropertySpec
    )
}
