package io.github.arainko

import io.github.arainko.model.cli._
import io.github.arainko.model.errors._
import zio._
import zio.console._
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.charset.Charset
import zio.nio.core.file.Path
import zio.stream.ZStream

import java.nio.file.StandardOpenOption

object Main extends App {

  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val cipherArg = args.headOption
      .toRight(ArgumentsMissingError)
      .flatMap(CipherArgument.fromString)

    val subargument = args.drop(1).headOption
      .toRight(ArgumentsMissingError)
      .flatMap(Subargument.fromString)

    val program = for {
      cipher <- ZIO.fromEither(cipherArg).tapError(err => putStrLnErr(err.toString))
      subarg <- ZIO.fromEither(subargument).tapError(err => putStrErr(err.toString))
      _ <- dispatch(cipher, subarg).flatMap(putStrLn(_))
    } yield ()

    program.fold(_ => ExitCode.failure, _ => ExitCode.success)
  }

  private def dispatch(cipherArg: CipherArgument, subarg: Subargument) = ZIO.succeed(s"$cipherArg $subarg")

  // /home/aleksander/IdeaProjects/caesar-n-affine/src/main/resources/plain.txt

  private def readFile(charset: Charset, file: Path) =
    AsynchronousFileChannel
      .open(file, StandardOpenOption.READ, StandardOpenOption.CREATE)
      .use { channel =>
        ZStream
          .fromIterableM {
            channel.size.flatMap(size => channel.readChunk(size.toInt, 0L))
          }
          .transduce(charset.newDecoder.transducer())
          .runCollect
          .map(_.mkString)
      }

}
