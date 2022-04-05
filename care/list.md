% Care list
% siiky
% 2022/01/09

(If you don't know what IPFS is or don't know how to use it, see
[here](/care/ipfs.html))

IPFS root directory of this list: ipfs://bafybeicsuo27ufnhjw2k5ophno5hgxhuvfbzzerrgsx7fxmy4kxwthpkbi

Because _sharing is caring_. This is a list of things I'm caring with others.
Some of the type of cares you may find here are books from [Gutenberg] or
[Standard Ebooks] that I particularly like; public movies/documentaries I find
interesting, important, and/or important (such as _Steal this Film_); and
copies of technical material distributed publicly and freely by their creators
(such as OPLSS lectures). Whatever care you may find here, it shouldn't be
illegal.

You'll find at least three spider cares.

![Three Spider cares](assets/spidercare.jpg)

The URL of the root of the list will be always at the top of the page (the
"IPFS root directory ..."). From it you can get to all other cares (if I don't
screw up). Over the rest of the page I'll post some notable cares and/or
(maybe) more details, maybe even sources.

The cares are added to IPFS using `ipfs add` with the options `--recursive
--raw-leaves --cid-version=1`. Additionally, videos should also get a
`--trickle `. The full command is something like this:

```
ipfs files cp /ipfs/`ipfs add -p -Q -r -t --pin=false --cid-version=1 --raw-leaves '/path/to/file_or_directory'` '/care-list/videos/file_or_directory'
```

If you feel the urge to support someone after finding or benefitting from this
list, support the creators of the cares themselves.

# Books

## _Geographies of Digital Exclusion: Data and Inequality_

Found in `/books/geographies_of_digital_exclusion.pdf`. Originally downloaded
from the website of the [Oxford Internet
Institute](https://www.oii.ox.ac.uk/research/publications/geographies-of-digital-exclusion-data-and-inequality).

Published in 2022/01 by Pluto Press, with ISBN 9780745340180.

## _Program Design by Calculation_

Written by José Nuno Oliveira, professor of the Informatics Department of the
University of Minho.

At `/books/pdbc.pdf` (not necessarily the latest version), and the original is
usually available [here][pdbc].

# Videos

## OPLSS (Oregon Programming Language Summer School)

The [official OPLSS site](https://www.cs.uoregon.edu/research/summerschool/archives.html).

Some of the lectures are mirrored on the path `/videos/oplss/`, or here:
ipfs://bafybeidlgjvd6qdjgdb2vqtdnssvvfbv47m3wj2uyzknswhj5twktr37ne

**NOTE THAT I _may_ forget to update this URL, but the root will always be up
to date!**

## _Nothing to Hide_

 * [VidCommons' PeerTube instance](https://vidcommons.org/w/efeEpsHSK3bzJwVLW9fh7U)
 * `/videos/Nothing to Hide - The documentary about surveillance and you (2017) 1080p.mp4`
 * magnet:?xt=urn:btih:34d4615762a92083aaf892630e84284c3ebaf3e5&dn=Nothing%20to%20Hide%20-%20The%20documentary%20about%20surveillance%20and%20you%20%282017%29%201080p.mp4&tr=https%3A%2F%2Fvidcommons.org%2Ftracker%2Fannounce&ws=https%3A%2F%2Fvidcommons.org%2Fstatic%2Fwebseed%2F6b43a28c-86ee-4681-8ab9-787ac140afc0-1080.mp4
 * ipfs://bafybeidi44wekwu3bi24m5x6ofnttzbcsfomr4dw5bssovqq35wdwva26m?filename=Nothing%20to%20Hide%20-%20The%20documentary%20about%20surveillance%20and%20you%20(2017)%201080p.mp4

## _TPB: AFK_

Torrent: magnet:?xt=urn:btih:411a7a164505636ab1a8276395b375a3a30bff32&dn=TPB.AFK.2013.1080p.h264-SimonKlose&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969%2Fannounce&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A6969%2Fannounce&tr=udp%3A%2F%2F9.rarbg.to%3A2710%2Fannounce&tr=udp%3A%2F%2F9.rarbg.me%3A2780%2Fannounce&tr=udp%3A%2F%2F9.rarbg.to%3A2730%2Fannounce&tr=udp%3A%2F%2Ftracker.opentrackr.org%3A1337&tr=http%3A%2F%2Fp4p.arenabg.com%3A1337%2Fannounce&tr=udp%3A%2F%2Ftracker.torrent.eu.org%3A451%2Fannounce&tr=udp%3A%2F%2Ftracker.tiny-vps.com%3A6969%2Fannounce&tr=udp%3A%2F%2Fopen.stealth.si%3A80%2Fannounce

And on IPFS at `/videos/TPB.AFK.2013.1080p.h264-SimonKlose.mkv`.

## _Steal this Film_

Torrents from the [official site](https://www.stealthisfilm.com):

 1. magnet:?xt=urn:btih:2d71be0c1bcc93373e926f02aaa4ab484979a542&dn=StealThisFilm.Part1.mov&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80%2Fannounce
 2. magnet:?xt=urn:btih:0ff9cd4f43f5df2a30c694c87e63978af26ee9d4&dn=Steal%20This%20Film%20II.720p.mov&tr=udp%3A%2F%2Ftracker.openbittorrent.com%3A80%2Fannounce

On IPFS:

 1. `/videos/StealThisFilm.Part1.mov`
 1. `/videos/StealThisFilmII.720p.mov`

## _O Lado Negro das Energias Verdes_

This is the documentary [_The Dark Side of Green Energies_], but in PT
(narration & subtitles) from RTP. Available at
`/videos/o_lado_negro_das_energias_verdes.mp4`.

[Gutenberg]: https://www.gutenberg.org
[Standard Ebooks]: https://standardebooks.org
[_The Dark Side of Green Energies_]: https://www.imdb.com/title/tt13524468
[pdbc]: http://www4.di.uminho.pt/~jno/ps/pdbc.pdf
