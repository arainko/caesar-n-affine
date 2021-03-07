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

    override def decode(crypto: Crypto, key: Key.Caesar): Plain = ???

    override def crackWithExtra(text: Crypto, extra: Extra): Either[ImpossibleToDecipher, (Key.Caesar, Plain)] = ???

    override def crack(text: Crypto): List[Plain] = ???

    private def encodeRelative(alphabetStart: Char, charToEncode: Char, key: Key.Caesar) = {
      val charCode         = charToEncode.toInt
      val offsetCharCode   = charCode - alphabetStart
      val moduloCharCode   = (offsetCharCode + key.offset) % 26
      val alphabetCharCode = alphabetStart + moduloCharCode
      alphabetCharCode.toChar
    }

    private def encoded(key: Key.Caesar)(char: Char) =
      char match {
        case numeric if numeric.isDigit            => Some(numeric)
        case punct if isPunctuation(punct)         => Some(punct)
        case whitespace if whitespace.isWhitespace => Some(whitespace)
        case lowerChar if lowerCaseAlphabet.contains(lowerChar) =>
          val encodedChar = encodeRelative('a', lowerChar, key)
          Some(encodedChar)
        case upperChar if upperCaseAlphabet.contains(upperChar) =>
          val encodedChar = encodeRelative('A', upperChar, key)
          Some(encodedChar)
        case _ => None
      }

  }
}
