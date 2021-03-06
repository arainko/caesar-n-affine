package io.github.arainko

import zio._
import zio.console._
import zio.nio.channels.AsynchronousFileChannel
import zio.nio.core.charset.Charset
import zio.nio.core.file.Path
import zio.stream.ZStream

import java.nio.file.StandardOpenOption
import io.github.arainko.model.errors._
import io.github.arainko.model.program._
import zio.internal._

/**
 * Dumps a text file to the console using a specified encoding.
 *
 * Two command line parameters must be provided:
 * 1. The path of the file to dump
 * 2. The character encoding to use — optional, defaults to UTF-8
 */
object TextFileDump extends App {

  override val platform: Platform = Platform.benchmark
    .withReportFailure(_ => ())
    .withTracing(Tracing.disabled)

  override def run(args: List[String]): URIO[ZEnv, ExitCode] = {
    val cipherArg = args.headOption
      .toRight(ArgumentsMissingError)
      .flatMap(CipherArgument.fromString)

    val subargument = args.tail.headOption
      .toRight(ArgumentsMissingError)
      .flatMap(Subargument.fromString)

    val program = for {
      cipher <- ZIO.fromEither(cipherArg)
      subarg <- ZIO.fromEither(subargument)
      // cos = cipher.
      _ <- dispatch(cipher, subarg).flatMap(putStrLn(_))
      // fileArg  <- getStrLn
      // asd <- ZStream.fromFile(Paths.get(fileArg))
      //   .transduce(Charset.Standard.utf8.newDecoder.transducer())
      //   .runCollect
      //   .map(_.mkString)
      //   .either
      // _        <- asd.fold(err => putStrErr(err.toString()), res => putStr(res))
    } yield ()

    program.exitCode
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
