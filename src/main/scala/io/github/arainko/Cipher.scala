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

  val caesar = new Cipher[Key.Caesar] {
    private val lowerCaseAlphabet = ('a' to 'z').toSet
    private val upperCaseAlphabet = ('A' to 'Z').toSet

    private val punctuationRegex          = "\\p{Punct}".r
    private def isPunctuation(char: Char) = punctuationRegex.matches(char.toString)

    override def encode(plain: Plain, key: Key.Caesar): Crypto =
      Crypto {
        plain.text
          .flatMap(encoded(key))
          .foldLeft("")(_ + _.toString)
      }

    override def decode(crypto: Crypto, key: Key.Caesar): Plain =
      Plain {
        crypto.text
          .flatMap(decoded(key))
          .foldLeft("")(_ + _.toString)
      }

    override def crackWithExtra(crypto: Crypto, extra: Extra): Either[ImpossibleToDecipher, (Key.Caesar, Plain)] =
      extra.text.zipWithIndex
        .filter { case (char, _) => (lowerCaseAlphabet ++ upperCaseAlphabet).contains(char) }
        .flatMap { case (char, index) =>
          Option.when(crypto.text.isDefinedAt(index))(Key.Caesar(crypto.text(index).toInt - char.toInt))
        }
        .headOption
        .map(key => key -> decode(crypto, key))
        .toRight(ImpossibleToDecipherError)

    override def crack(text: Crypto): List[Plain] = (1 to 25).map(Key.Caesar.andThen(decode(text, _))).toList

    private def relativeCode(alphabetStart: Char, char: Char, key: Key.Caesar, f: (Int, Int) => Int) = {
      val charCode         = char.toInt
      val offsetCharCode   = charCode - alphabetStart
      val moduloCharCode   = Math.floorMod(f(offsetCharCode, key.offset), 26)
      val alphabetCharCode = alphabetStart + moduloCharCode
      alphabetCharCode.toChar
    }

    private def process(key: Key.Caesar, f: (Int, Int) => Int)(char: Char) =
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

    private def decoded(key: Key.Caesar)(char: Char) = process(key, _ - _)(char)
    private def encoded(key: Key.Caesar)(char: Char) = process(key, _ + _)(char)

  }
}
