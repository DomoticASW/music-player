# A Music Player simulated device for DomoticASW

## Run with docker

```sh
docker run corradostortini2/domoticasw-music-player
```

The following configurations can be passed to the container as environment variables.

For example `docker run -e SERVER_DISCOVERY_PORT=30000 -e MUSICS="Monster-10,Chop Suey!-20,La follia di Vivaldi-50" corradostortini2/domoticasw-music-player`

| Variable name            | Default value                                            | Explanation                                          | Admissible values                               |
| ------------------------ | -------------------------------------------------------- | ---------------------------------------------------- | ----------------------------------------------- |
| ID                       | music-player                                             | Music player id                                      | Any not empty string                            |
| NAME                     | Music Player                                             | Music player name                                    | Any not empty string                            |
| UPDATE_RATE              | 1000                                                     | Music's state update interval (ms)                   | Integers > 0                                    |
| MUSICS                   | Black In Black-10, Don't stop believin-5, Poker's Face-7 | Musics in the player                                 | Comma-separated list of musicName-duration      |
| DISCOVERY_BROADCAST_ADDR | 255.255.255.255                                          | Broadcast address to which send discovery announces  | Any valid broadcast address (ex: 192.168.1.255) |
| SERVER_DISCOVERY_PORT    | 30000                                                    | Port on which the server expects discovery announces | Any valid port                                  |
| SERVER_ADDRESS           | None                                                     | Should be set if music player is already registered  |                                                 |
| PORT                     | 8080                                                     | Port on which the device will listen                 | Any valid port                                  |

## Properties

The properties available on the music player are:

- state (the current music state: Playing, Paused, Off)
- music (the current music being played/paused/off)
- minutes (the minutes of the current song, rappresented as a couple of minutes:seconds e.g.01:05/01:20 means 1 minute and 5 seconds passed, while the duration of the current music is of 1 minute and 20 seconds)
- music-progress (the progress in percentage of the current music, can be changed to go back and forth through the music)

## Events

The events available on the music player are:

- ChangeMusic (the music has changed)
- Start (the music just started from 0)
- Resume (the music resumed)
- Pause (the music is paused)
- End (the music finished)

## Actions

The actions available on the music player are:

- play (starts the song if it is not finished)
- pause (pause the song at the current minutes)
- stop (stop the song, setting the minutes to 00:00)
- change-music (change the current music to a new music starting from 00:00 and keeping the last music state)
- set-music-progress (change the minutes of the current song)
