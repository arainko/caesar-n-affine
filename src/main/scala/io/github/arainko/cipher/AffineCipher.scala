package io.github.arainko.cipher

import io.github.arainko.model.common._
import io.github.arainko.model.errors.{ImpossibleToDecipher, ImpossibleToDecipherError}

class AffineCipher(
  lowerCaseAlphabet: Set[Char],
  upperCaseAlphabet: Set[Char],
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
      .flatMap {
        case (char, index) =>
          Option.when(crypto.text.isDefinedAt(index))(crypto.text(index) -> char)
      }
      .take(2)

    for {
      refinedCandidates <- Either.cond(candidates.size == 2, candidates, ImpossibleToDecipherError)
      (result1, alpha1) = refinedCandidates.head
      (result2, alpha2) = refinedCandidates.tail.head
      equation1         = Equation(alpha1.toInt, 1, result1.toInt)
      equation2         = Equation(alpha2.toInt, 1, result2.toInt)
      key <- solveForAlphaAndBetaInRing26(equation1, equation2)
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

  private def solveForAlphaAndBetaInRing26(equation1: Equation, equation2: Equation) =
    for {
      _ <- Either.cond(equation1.beta == 1, (), ImpossibleToDecipherError)
      _ <- Either.cond(equation2.beta == 1, (), ImpossibleToDecipherError)
      reduced = (equation1 - equation2).mod(26)
      inverseAlpha <- Either.cond(
        coprimes.contains(reduced.alpha),
        inverseInRing(26)(reduced.alpha),
        ImpossibleToDecipherError
      )
      alphaValue = Math.floorMod(reduced.result * inverseAlpha, 26)
      nonModBeta = equation1.result - alphaValue * equation1.alpha
      betaValue  = Math.floorMod(nonModBeta, 26)
    } yield Key.Affine.Validated(betaValue, alphaValue)

}
