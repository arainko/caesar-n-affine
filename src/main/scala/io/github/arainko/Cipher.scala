package io.github.arainko

import io.github.arainko.model.cli._
import io.github.arainko.model.common._
import io.github.arainko.model.errors._

trait Cipher[K <: Key] {
  def encode(plain: Plain, key: K): Crypto
  def decode(crypto: Crypto, key: K): Plain
  def crackWithExtra(text: Crypto, extra: Extra): Either[ImpossibleToDecipher, (K, Plain)]
  def crack(text: Crypto): List[Plain]
}

object Cipher {

  private val lowerCaseAlphabet         = ('a' to 'z').toSet
  private val upperCaseAlphabet         = ('A' to 'Z').toSet
  private val punctuationRegex          = "\\p{Punct}".r
  private def isPunctuation(char: Char) = punctuationRegex.matches(char.toString)

  val caesar = new Cipher[Key.Caesar.Validated] {

    override def encode(plain: Plain, key: Key.Caesar.Validated): Crypto =
      Crypto {
        plain.text
          .flatMap(encoded(key))
          .foldLeft("")(_ + _.toString)
      }

    override def decode(crypto: Crypto, key: Key.Caesar.Validated): Plain =
      Plain {
        crypto.text
          .flatMap(decoded(key))
          .foldLeft("")(_ + _.toString)
      }

    override def crackWithExtra(
      crypto: Crypto,
      extra: Extra
    ): Either[ImpossibleToDecipher, (Key.Caesar.Validated, Plain)] =
      extra.text.zipWithIndex
        .filter { case (char, _) => (lowerCaseAlphabet ++ upperCaseAlphabet).contains(char) }
        .flatMap { case (char, index) =>
          Option.when(crypto.text.isDefinedAt(index))(Key.Caesar.Validated(crypto.text(index).toInt - char.toInt))
        }
        .headOption
        .map(key => key -> decode(crypto, key))
        .toRight(ImpossibleToDecipherError)

    override def crack(text: Crypto): List[Plain] = (1 to 25).map(Key.Caesar.Validated.andThen(decode(text, _))).toList

    private def relativeCode(alphabetStart: Char, char: Char, key: Key.Caesar.Validated, f: (Int, Int) => Int) = {
      val charCode         = char.toInt
      val offsetCharCode   = charCode - alphabetStart
      val moduloCharCode   = Math.floorMod(f(offsetCharCode, key.offset), 26)
      val alphabetCharCode = alphabetStart + moduloCharCode
      alphabetCharCode.toChar
    }

    private def process(key: Key.Caesar.Validated, f: (Int, Int) => Int)(char: Char) =
      char match {
        case numeric if numeric.isDigit            => Some(numeric)
        case punct if isPunctuation(punct)         => Some(punct)
        case whitespace if whitespace.isWhitespace => Some(whitespace)
        case lowerChar if lowerCaseAlphabet.contains(lowerChar) =>
          val decodedChar = relativeCode('a', lowerChar, key, f)
          Some(decodedChar)
        case upperChar if upperCaseAlphabet.contains(upperChar) =>
          val decodedChar = relativeCode('A', upperChar, key, f)
          Some(decodedChar)
        case _ => None
      }

    private def decoded(key: Key.Caesar.Validated)(char: Char) = process(key, _ - _)(char)
    private def encoded(key: Key.Caesar.Validated)(char: Char) = process(key, _ + _)(char)

  }

  val affine = new Cipher[Key.Affine.Validated] {

    private val coprimes = Set(1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25)

    private def inverseInRing(ring: Int)(value: Int) =
      (0 until ring)
        .find(elem => Math.floorMod(elem * value, ring) == 1)
        .get

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
      text: Crypto,
      extra: Extra
    ): Either[ImpossibleToDecipher, (Key.Affine.Validated, Plain)] = ???

    override def crack(text: Crypto): List[Plain] =
      for {
        offset <- (1 to 26).toList
        factor <- coprimes
        key = Key.Affine.Validated(offset, factor)
      } yield decode(text, key)

    private def relativeCode(alphabetStart: Char, char: Char, key: Key.Affine.Validated, f: (Int, Int, Int) => Int) = {
      val charCode         = char.toInt
      val offsetCharCode   = charCode - alphabetStart
      val moduloCharCode   = Math.floorMod(f(key.factor, offsetCharCode, key.offset), 26)
      val alphabetCharCode = alphabetStart + moduloCharCode
      alphabetCharCode.toChar
    }

    private def process(key: Key.Affine.Validated)(f: (Int, Int, Int) => Int)(char: Char) =
      char match {
        case numeric if numeric.isDigit            => Some(numeric)
        case punct if isPunctuation(punct)         => Some(punct)
        case whitespace if whitespace.isWhitespace => Some(whitespace)
        case lowerChar if lowerCaseAlphabet.contains(lowerChar) =>
          val decodedChar = relativeCode('a', lowerChar, key, f)
          Some(decodedChar)
        case upperChar if upperCaseAlphabet.contains(upperChar) =>
          val decodedChar = relativeCode('A', upperChar, key, f)
          Some(decodedChar)
        case _ => None
      }

    private def decoded(key: Key.Affine.Validated)(char: Char) =
      process(key) { (factor, charCode, offset) =>
        val inverseFactor = inverseInRing(26)(factor)
        inverseFactor * (charCode - offset)
      }(char)

    private def encoded(key: Key.Affine.Validated)(char: Char) = process(key)(_ * _ + _)(char)

  }
}
