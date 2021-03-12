package io.github.arainko.model

import io.github.arainko.model.files.File._

import java.nio.file.{Path, Paths}

object files {

  sealed trait File {

    def path: Path =
      this match {
        case Crypto   => Paths.get("files/crypto.txt")
        case Decrypt  => Paths.get("files/decrypt.txt")
        case Extra    => Paths.get("files/extra.txt")
        case KeyFound => Paths.get("files/key-found.txt")
        case Key      => Paths.get("files/key.txt")
        case Plain    => Paths.get("files/plain.txt")
      }
  }

  object File {
    case object Crypto   extends File
    case object Decrypt  extends File
    case object Extra    extends File
    case object KeyFound extends File
    case object Key      extends File
    case object Plain    extends File
  }
}
