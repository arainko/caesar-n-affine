package io.github.arainko.cipher

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

  val caesar = new CaesarCipher(lowerCaseAlphabet, upperCaseAlphabet)
  val affine = new AffineCipher(lowerCaseAlphabet, upperCaseAlphabet)

  private[cipher] def process[K <: Key](key: K)(f: (Int, K) => Int)(char: Char) =
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

  private def relativeCode[K <: Key](alphabetStart: Char, char: Char, key: K, f: (Int, K) => Int) = {
    val charCode         = char.toInt
    val offsetCharCode   = charCode - alphabetStart
    val moduloCharCode   = Math.floorMod(f(offsetCharCode, key), 26)
    val alphabetCharCode = alphabetStart + moduloCharCode
    alphabetCharCode.toChar
  }

}
