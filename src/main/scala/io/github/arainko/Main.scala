package io.github.arainko

import io.github.arainko.cipher.CipherM
import io.github.arainko.model.cli.CipherArgument.{Affine, Caesar}
import io.github.arainko.model.cli.Subargument._
import io.github.arainko.model.cli._
import io.github.arainko.model.errors._
import zio._
import zio.console._

object Main extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val cipherArg = args.headOption
      .toRight(ArgumentsMissingError)
      .flatMap(CipherArgument.fromString)

    val subargument = args
      .drop(1)
      .headOption
      .toRight(ArgumentsMissingError)
      .flatMap(Subargument.fromString)

    val program = for {
      cipher <- ZIO.fromEither(cipherArg).tapError(err => putStrLnErr(err.toString))
      subarg <- ZIO.fromEither(subargument).tapError(err => putStrLnErr(err.toString))
      _      <- dispatch(cipher, subarg).tapError(err => putStrLnErr(err.toString))
    } yield ()

    program.fold(_ => ExitCode.failure, _ => ExitCode.success)
  }

  private def dispatch(cipherArg: CipherArgument, subarg: Subargument) =
    cipherArg match {
      case Caesar =>
        subarg match {
          case Decode             => CipherM.decodeCaesar
          case Encode             => CipherM.encodeCaesar
          case AnalysisWithCrypto => CipherM.crackCaesar
          case AnalysisWithExtra  => CipherM.crackWithPlainCaesar
        }
      case Affine =>
        subarg match {
          case Decode             => CipherM.decodeAffine
          case Encode             => CipherM.encodeAffine
          case AnalysisWithCrypto => CipherM.crackAffine
          case AnalysisWithExtra  => CipherM.crackWithPlainAffine
        }
    }

}
