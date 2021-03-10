package io.github.arainko.model

object common {
  final case class Plain(text: String)   extends AnyVal
  final case class Extra(text: String)   extends AnyVal
  final case class Crypto(text: String)  extends AnyVal
  final case class Decrypt(text: String) extends AnyVal

  sealed trait Key

  object Key {

    sealed trait Caesar extends Key

    object Caesar {
      final case class Unvalidated(offset: Int) extends Caesar
      final case class Validated(offset: Int)   extends Caesar
    }

    sealed trait Affine extends Key

    object Affine {
      final case class Unvalidated(offset: Int, factor: Int) extends Affine
      final case class Validated(offset: Int, factor: Int)   extends Affine
    }
  }
}
