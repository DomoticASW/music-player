package domain

import utils.OneOf
import OneOf.*
import domain.MusicPlayer.MusicPlayerOpsImpl.MusicState
import logger.LoggerImpl.LoggerState
import sleeper.SleeperImpl.SleeperState

type GlobalState = More[MusicState, More[LoggerState, One[SleeperState]]]
