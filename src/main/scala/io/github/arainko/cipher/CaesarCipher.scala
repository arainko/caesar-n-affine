package io.github.arainko.cipher

import io.github.arainko.model.common._
import io.github.arainko.model.errors._

class CaesarCipher(
  lowerCaseAlphabet: Set[Char],
  upperCaseAlphabet: Set[Char]
) extends Cipher[Key.Caesar.Validated] {

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
        Option.when(crypto.text.isDefinedAt(index))(Key.Caesar.Validated(crypto.text(index).toLower - char.toLower))
      }
      .headOption
      .map { key =>
        val normalizedOffset = Math.floorMod(key.offset, 26)
        val normalizedKey    = Key.Caesar.Validated(normalizedOffset)
        normalizedKey -> decode(crypto, key)
      }
      .toRight(ImpossibleToDecipherError)

  override def crack(text: Crypto): List[Plain] = (1 to 25).map(Key.Caesar.Validated.andThen(decode(text, _))).toList

  private def decoded(key: Key.Caesar.Validated)(char: Char) = Cipher.process(key)(_ - _.offset)(char)

  private def encoded(key: Key.Caesar.Validated)(char: Char) = Cipher.process(key)(_ + _.offset)(char)
}
