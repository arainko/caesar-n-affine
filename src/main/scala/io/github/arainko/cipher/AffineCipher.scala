package io.github.arainko.cipher

import io.github.arainko.model.common._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import io.github.arainko.model.errors.{ImpossibleToDecipher, ImpossibleToDecipherError}

//TODO: Fix decrypt with extra

class AffineCipher(
  lowerCaseAlphabet: Set[Char],
  upperCaseAlphabet: Set[Char]
) extends Cipher[Key.Affine.Validated] {

  private val coprimes = Set(1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25)

  override def encode(plain: Plain, key: Key.Affine.Validated): Crypto =
    Crypto {
      plain.text
        .flatMap(encoded(key))
        .foldLeft("")(_ + _.toString)
    }

  override def decode(crypto: Crypto, key: Key.Affine.Validated): Plain =
    Plain {
      crypto.text
        .flatMap(decoded(key))
        .foldLeft("")(_ + _.toString)
    }

  override def crackWithExtra(
    crypto: Crypto,
    extra: Extra
  ): Either[ImpossibleToDecipher, (Key.Affine.Validated, Plain)] = {
    val candidates = extra.text.zipWithIndex
      .filter { case (char, _) => (lowerCaseAlphabet ++ upperCaseAlphabet).contains(char) }
      .flatMap { case (char, index) =>
        Option.when(crypto.text.isDefinedAt(index))(crypto.text(index).toLower -> char.toLower)
      }
      .take(2)
      .map(_.bimap(_ - 97, _ - 97))

    for {
      refinedCandidates <- Either.cond(candidates.size == 2, candidates, ImpossibleToDecipherError)
      (crypto1, extra1) = refinedCandidates.head
      (crypto2, extra2) = refinedCandidates.tail.head
      inverse = inverseInRing(26)((extra1 - extra2).abs)
      factor = Math.floorMod(inverse * (crypto1 - crypto2), 26) 
      offset = Math.floorMod(inverse * (extra1 * crypto2 - extra2 * crypto1), 26)
      key = Key.Affine.Validated(offset, factor)
    } yield key -> decode(crypto, key)
  }

  override def crack(text: Crypto): List[Plain] =
    for {
      offset <- (1 to 26).toList
      factor <- coprimes
      key = Key.Affine.Validated(offset, factor)
    } yield decode(text, key)

  private def decoded(key: Key.Affine.Validated)(char: Char) =
    Cipher.process(key) { (charCode, key) =>
      val inverseFactor = inverseInRing(26)(key.factor)
      inverseFactor * (charCode - key.offset)
    }(char)

  private def encoded(key: Key.Affine.Validated)(char: Char) =
    Cipher.process(key) { (charCode, key) =>
      key.factor * charCode + key.offset
    }(char)

  private def inverseInRing(ring: Int)(value: Int) =
    (0 until ring)
      .find(elem => Math.floorMod(elem * value, ring) == 1)
      .get
}
