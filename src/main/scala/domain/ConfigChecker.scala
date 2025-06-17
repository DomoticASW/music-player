package domain

import utils.Eithers.leftIf

case class BadConfiguration(message: String)

case class Config(id: String, name: String, musics: Set[Music], steps: Long)

object ConfigChecker:
  def apply(id: String, name: String, musics: Set[Music], steps: Long): Either[BadConfiguration, Config] =
    for
      _ <- Either.leftIf(
          steps > 0,
          BadConfiguration("Given steps should be greater than 0")
        )
    yield Config(id = id, name = name, musics = musics, steps = steps)