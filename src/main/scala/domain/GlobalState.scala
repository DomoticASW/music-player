package domain

import utils.OneOf
import OneOf.*
import domain.MusicPlayerOpsImpl.MusicState
import log.LoggerImpl.LoggerState
import sleep.SleeperImpl.SleeperState

type GlobalState = More[MusicState, More[LoggerState, One[SleeperState]]]
