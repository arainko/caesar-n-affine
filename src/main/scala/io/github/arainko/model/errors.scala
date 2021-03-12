package io.github.arainko.model

object errors {
  sealed trait ApplicationError

  sealed trait CliError extends ApplicationError

  case object ArgumentsMissingError                     extends CliError
  final case class CipherArgumentError(message: String) extends CliError
  final case class SubargumentError(message: String)    extends CliError

  sealed trait FileError extends ApplicationError

  final case class FileMissingError(message: String) extends FileError
  final case class FileFormatError(message: String)  extends FileError

  sealed trait CipherError extends ApplicationError
  type ImpossibleToDecipher = ImpossibleToDecipherError.type

  final case class BadKeyError(message: String) extends CipherError
  case object ImpossibleToDecipherError         extends CipherError

}
