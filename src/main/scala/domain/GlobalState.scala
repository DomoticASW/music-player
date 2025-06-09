package domain

import utils.OneOf
import OneOf.*
import domain.MusicPlayerOpsImpl.MusicState
import log.LoggerImpl.LoggerState

type GlobalState = More[MusicState, One[LoggerState]]
