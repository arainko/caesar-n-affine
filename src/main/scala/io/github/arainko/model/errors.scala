package io.github.arainko.model

import scala.util.control.NoStackTrace

object errors {
  sealed trait ApplicationError

  case object ArgumentsMissingError                     extends ApplicationError
  final case class CipherArgumentError(message: String) extends ApplicationError
  final case class SubargumentError(message: String)    extends ApplicationError
  final case class FileMissingError(message: String)    extends ApplicationError
}
