package domain

import utils.Eithers.leftIf

case class BadConfiguration(message: String)

case class Config(name: String, musics: Set[Music], steps: Int, p: Double)

object ConfigChecker:
  def apply(name: String, musics: Set[Music], steps: Int, p: Double): Either[BadConfiguration, Config] =
    for
      _ <- Either.leftIf(
          steps > 0,
          BadConfiguration("Given steps should be greater than 0")
        )
      _ <- Either.leftIf(
          p >= 0.0 && p <= 1.0,
          BadConfiguration("Given probability should be between 0.0 and 1.0")
        )
    yield Config(name = name, musics = musics, steps = steps, p = p)