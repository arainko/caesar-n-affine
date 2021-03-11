package io.github.arainko.model

import cats.syntax.all._
import cats.instances.either._
import cats.instances.option._
import cats.instances.list._
import io.github.arainko.model.errors._

object common {
  final case class Plain(text: String)   extends AnyVal
  final case class Extra(text: String)   extends AnyVal
  final case class Crypto(text: String)  extends AnyVal
  final case class Decrypt(text: String) extends AnyVal

  sealed trait Key

  object Key {

    sealed trait Caesar extends Key

    object Caesar {

      def fromString(string: String): Either[BadKeyError, Unvalidated] =
        string.toIntOption
          .map(Key.Caesar.Unvalidated)
          .toRight(BadKeyError("Malformed Caesar cipher key (not an integer)"))

      final case class Unvalidated(offset: Int) extends Caesar {

        def validated: Either[BadKeyError, Validated] =
          Either.cond(
            offset > 0 && offset < 26,
            Key.Caesar.Validated(offset),
            BadKeyError("Caesar key should be an integer between 1 and 25")
          )
      }

      final case class Validated(offset: Int) extends Caesar
    }

    sealed trait Affine extends Key

    object Affine {
      private val coprimes = List(1, 3, 5, 7, 9, 11, 15, 17, 19, 21, 23, 25)
      private val errorMessage = "Affine key is constructed from two integer values (the offset and the factor)"

      def fromString(string: String): Either[BadKeyError, Unvalidated] =
        for {
          keyList <- string
            .split(' ')
            .toList
            .traverse(_.toIntOption.toRight(BadKeyError(errorMessage)))
          unvalidatedKey <- (keyList.headOption -> keyList.drop(1).headOption)
            .mapN(Key.Affine.Unvalidated)
            .toRight(BadKeyError(errorMessage))
        } yield unvalidatedKey

      final case class Unvalidated(offset: Int, factor: Int) extends Affine {

        def validated: Either[BadKeyError, Validated] =
          Either.cond(
            offset > 0 && offset < 26 && coprimes.contains(factor),
            Key.Affine.Validated(offset, factor),
            BadKeyError(
              "Affine key's offset should be an integer between 1 and 25 and its factor should form a coprime number in the ring of 26"
            )
          )
      }
      final case class Validated(offset: Int, factor: Int) extends Affine
    }
  }

  case class Equation(alpha: Int, beta: Int, result: Int) {
    def unary_- : Equation           = Equation(-alpha, -beta, -result)
    def + (that: Equation): Equation = Equation(alpha + that.alpha, beta + that.beta, result + that.result)
    def - (that: Equation): Equation = this + (-that)

    def mod(value: Int): Equation =
      Equation(Math.floorMod(alpha, value), Math.floorMod(beta, value), Math.floorMod(result, value))
  }
}
