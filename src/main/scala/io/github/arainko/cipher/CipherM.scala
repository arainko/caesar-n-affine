package io.github.arainko.cipher

import zio._
import zio.stream._
import io.github.arainko.model.common._
import zio.nio.channels.AsynchronousFileChannel
import java.nio.file.StandardOpenOption
import io.github.arainko.model.errors._
import java.nio.file.Paths
import java.nio.file.Path
import java.nio.charset.StandardCharsets

object CipherM {

  // private def readFile(charset: Charset, file: Path) =
  //   AsynchronousFileChannel
  //     .open(file, StandardOpenOption.READ, StandardOpenOption.CREATE)
  //     .use { channel =>
  //       ZStream
  //         .fromIterableM {
  //           channel.size.flatMap(size => channel.readChunk(size.toInt, 0L))
  //         }
  //         .transduce(charset.newDecoder.transducer())
  //         .runCollect
  //         .map(_.mkString)
  //     }

  private def readResouce(path: String) =
    ZStream.fromResource(path)
    .transduce(Transducer.utf8Decode)
    .runCollect
    .map(_.mkString)
    .mapError(err => FileMissingError(err.getMessage))

  private def writeToFile(content: String, path: String) =
    ZStream.fromIterable(content.getBytes(StandardCharsets.UTF_8))
    .run(Sink.fromFile(Paths.get(path)), StandardOpenOption)
    .orDie
    

  def encodeCaesar = 
    for {
       stringKey <- readResouce("key.txt")
       plainText <- readResouce("plain.txt").map(Plain)
       validatedKey <- ZIO.fromEither(Key.Caesar.fromString(stringKey).flatMap(_.validated))
       encoded = Cipher.caesar.encode(plainText, validatedKey)
       _ <- writeToFile(encoded.text, "/home/aleksander/ciphers/crypto")
    } yield ()
}
