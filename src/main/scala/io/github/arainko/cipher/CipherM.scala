package io.github.arainko.cipher

import io.github.arainko.model.common._
import io.github.arainko.model.errors._
import io.github.arainko.model.files._
import zio._
import zio.blocking.Blocking
import zio.stream._

import java.nio.charset.StandardCharsets

object CipherM {

  // CAESAR
  def encodeCaesar: ZIO[Blocking, ApplicationError, Unit] =
    for {
      stringKey    <- readFile(File.Key)
      plainText    <- readFile(File.Plain).map(Plain)
      validatedKey <- ZIO.fromEither(Key.Caesar.fromString(stringKey).flatMap(_.validated))
      encoded = Cipher.caesar.encode(plainText, validatedKey)
      _ <- writeToFile(encoded.text, File.Crypto)
    } yield ()

  def decodeCaesar: ZIO[Blocking, ApplicationError, Unit] =
    for {
      stringKey    <- readFile(File.Key)
      crypto       <- readFile(File.Crypto).map(Crypto)
      validatedKey <- ZIO.fromEither(Key.Caesar.fromString(stringKey).flatMap(_.validated))
      plain = Cipher.caesar.decode(crypto, validatedKey)
      _ <- writeToFile(plain.text, File.Decrypt)
    } yield ()

  def crackCaesar: ZIO[Blocking, ApplicationError, Unit] =
    for {
      crypto <- readFile(File.Crypto).map(Crypto)
      cracked = Cipher.caesar.crack(crypto).map(_.text).mkString("\n")
      _ <- writeToFile(cracked, File.Decrypt)
    } yield ()

  def crackWithPlainCaesar: ZIO[Blocking, ApplicationError, Unit] =
    for {
      crypto <- readFile(File.Crypto).map(Crypto)
      extra  <- readFile(File.Extra).map(Extra)
      cracked <- ZIO.fromEither {
        Cipher.caesar.crackWithExtra(crypto, extra)
      }
      (foundKey, plain) = cracked
      _ <- writeToFile(foundKey.offset.toString, File.KeyFound)
      _ <- writeToFile(plain.text, File.Decrypt)
    } yield ()

  // AFFINE
  def encodeAffine: ZIO[Blocking, ApplicationError, Unit] =
    for {
      stringKey    <- readFile(File.Key)
      plainText    <- readFile(File.Plain).map(Plain)
      validatedKey <- ZIO.fromEither(Key.Affine.fromString(stringKey).flatMap(_.validated))
      encoded = Cipher.affine.encode(plainText, validatedKey)
      _ <- writeToFile(encoded.text, File.Crypto)
    } yield ()

  def decodeAffine: ZIO[Blocking, ApplicationError, Unit] =
    for {
      stringKey    <- readFile(File.Key)
      crypto       <- readFile(File.Crypto).map(Crypto)
      validatedKey <- ZIO.fromEither(Key.Affine.fromString(stringKey).flatMap(_.validated))
      plain = Cipher.affine.decode(crypto, validatedKey)
      _ <- writeToFile(plain.text, File.Decrypt)
    } yield ()

  def crackAffine: ZIO[Blocking, ApplicationError, Unit] =
    for {
      crypto <- readFile(File.Crypto).map(Crypto)
      cracked = Cipher.affine.crack(crypto).map(_.text).mkString("\n")
      _ <- writeToFile(cracked, File.Decrypt)
    } yield ()

  def crackWithPlainAffine: ZIO[Blocking, ApplicationError, Unit] =
    for {
      crypto <- readFile(File.Crypto).map(Crypto)
      extra  <- readFile(File.Extra).map(Extra)
      cracked <- ZIO.fromEither {
        Cipher.affine.crackWithExtra(crypto, extra)
      }
      (foundKey, plain) = cracked
      _ <- writeToFile(s"${foundKey.offset} ${foundKey.factor}", File.KeyFound)
      _ <- writeToFile(plain.text, File.Decrypt)
    } yield ()

  private def readFile(file: File) =
    ZStream
      .fromFile(file.path)
      .transduce(Transducer.utf8Decode)
      .runCollect
      .map(_.mkString)
      .mapError(err => FileMissingError(err.getMessage))

  private def writeToFile(content: String, file: File) =
    ZStream
      .fromIterable(content.getBytes(StandardCharsets.UTF_8))
      .run(Sink.fromFile(file.path))
      .mapError(err => FileFormatError(err.getMessage))

}
