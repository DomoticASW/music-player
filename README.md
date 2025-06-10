# A Music Player simulated device for DomoticASW

## Run with docker

```sh
docker run corradostortini2/domoticasw-music-player
```

The following configurations can be passed to the container as environment variables.

For example `docker run -e MUSICS="Monster-10,Chop Suey!-20,La follia di Vivaldi-50" corradostortini2/domoticasw-music-player`


| Variable name | Default value                                                     | Explanation                             | Admissible values                                                    |
|---------------|-------------------------------------------------------------------|-----------------------------------------|----------------------------------------------------------------------|
| NAME          | Music Player                                                      | Music player name                       | Any not empty string                                                 |
| STEPS         | 1                                                                 | Music's state update interval (seconds) | Integers >= 1                                                        |
| MUSICS        | Black In Black - 100, Don't stop believin - 50, Poker's Face - 70 | Musics in the player                    | Comma-separated list of Yphen-separeted couple of musicName-duration |