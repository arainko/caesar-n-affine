package io.github.arainko.model

import io.github.arainko.model.errors._

object program {
  sealed trait CipherArgument

  object CipherArgument {

    def fromString(arg: String): Either[CipherArgumentError, CipherArgument] =
      arg match {
        case "-c"  => Right(CipherArgument.Caesar)
        case "-a"  => Right(CipherArgument.Affine)
        case wrong => Left(CipherArgumentError(s"'$wrong' is not a valid cipher arg, try either '-c' or '-a'"))
      }

    case object Caesar extends CipherArgument
    case object Affine extends CipherArgument
  }

  sealed trait Subargument

  object Subargument {

    def fromString(arg: String): Either[SubargumentError, Subargument] =
      arg match {
        case "-e" => Right(Subargument.Encode)
        case "-d" => Right(Subargument.Decode)
        case "-j" => Right(Subargument.AnalysisWithPlain)
        case "-k" => Right(Subargument.AnalysisWithCrypto)
        case wrong =>
          Left(SubargumentError(s"'$wrong' is not a valid subargument, try '-e', '-d', '-j' or '-k' instead"))
      }

    case object Encode             extends Subargument
    case object Decode             extends Subargument
    case object AnalysisWithPlain  extends Subargument
    case object AnalysisWithCrypto extends Subargument
  }

}
