# A Music Player simulated device for DomoticASW

## Run with docker

```sh
docker run corradostortini2/domoticasw-music-player
```

The following configurations can be passed to the container as environment variables.

For example `docker run -e MUSICS="Monster-10,Chop Suey!-20,La follia di Vivaldi-50" corradostortini2/domoticasw-music-player`


| Variable name  | Default value                                            | Explanation                        | Admissible values                                                    |
|--------------  |----------------------------------------------------------|----------------------------------- |----------------------------------------------------------------------|
| NAME           | Music Player                                             | Music player name                  | Any not empty string                                                 |
| STEPS          | 1000                                                     | Music's state update interval (ms) | Integers > 0                                                        |
| MUSICS         | Black In Black-10, Don't stop believin-5, Poker's Face-7 | Musics in the player               | Comma-separated list of musicName-duration |
| SERVER_ADDRESS | None | Should be set if music player is already registered | |
| PORT         | 8080 | Port on which the device will listen               | Any valid port |