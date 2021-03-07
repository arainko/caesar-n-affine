package io.github.arainko.model

object common {
  final case class Plain(text: String)   extends AnyVal
  final case class Extra(text: String)   extends AnyVal
  final case class Crypto(text: String)  extends AnyVal
  final case class Decrypt(text: String) extends AnyVal

  sealed trait Key

  object Key {

    final case class Caesar(private val _offset: Int) extends Key {
      lazy val offset = Math.floorMod(_offset, 26)
    }
    final case class Affine(offset: Int, factor: Int) extends Key
  }
}
