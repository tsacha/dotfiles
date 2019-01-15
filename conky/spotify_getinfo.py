#!/usr/bin/python
import dbus
import sys
import urllib.request
from pathlib import Path
import subprocess


workdir = Path(Path.home(), ".config/conky")
saved_artist = Path(workdir, ".saved_artist")
saved_song = Path(workdir, ".saved_song")
saved_album = Path(workdir, ".saved_album")
cover_file = Path(workdir, "cover.jpg")


def perfect_length(str, max_length=23):
    if len(str) > max_length:
        return str[:max_length] + "…"
    else:
        return str


def is_different_track(artist, album, song):
    return (
        (not saved_artist.is_file() or saved_artist.read_text() != artist)
        or (not saved_album.is_file() or saved_album.read_text() != album)
        or (not saved_song.is_file() or saved_song.read_text() != song)
    )


def purge():
    if saved_album.is_file():
        saved_album.unlink()
    if saved_artist.is_file():
        saved_artist.unlink()
    if saved_song.is_file():
        saved_song.unlink()
    if saved_cover.is_file():
        saved_cover.unlink()
    if cover_file.is_file():
        cover_file.unlink()


if __name__ == "__main__":
    if len(sys.argv[1]) <= 1:
        exit(0)

    elif sys.argv[1] == "make_cache":
        try:
            bus = dbus.SessionBus()

            proxy = bus.get_object(
                "org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2"
            )
            props_iface = dbus.Interface(
                proxy, dbus_interface="org.freedesktop.DBus.Properties"
            )
            metadata = props_iface.GetAll("org.mpris.MediaPlayer2.Player")["Metadata"]
            album = metadata["xesam:album"]
            song = metadata["xesam:title"]
            artist = metadata["xesam:artist"][0]
            cover = metadata["mpris:artUrl"]

        except Exception:
            purge()
            exit(0)

        if is_different_track(artist, album, song):
            saved_artist.write_text(artist)
            saved_song.write_text(song)
            saved_album.write_text(album)

            req = urllib.request.urlopen(cover)
            cover_file.write_bytes(req.read())

            cmd = f"convert -border 3 -bordercolor white {cover_file} {cover_file}"
            proc = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE)
            proc.wait()

    if sys.argv[1] == "i3bar":
        try:
            song = Path(workdir, ".saved_song").read_text()
            album = Path(workdir, ".saved_album").read_text()
            artist = Path(workdir, ".saved_artist").read_text()
            print(
                f"🎶 {perfect_length(song)} by {perfect_length(artist)} on {perfect_length(album)} 🎶"
            )
        except Exception:
            exit(0)
    else:
        try:
            info = Path(workdir, ".saved_" + sys.argv[1]).read_text()
            print(perfect_length(info, 40))
        except Exception:
            exit(0)
