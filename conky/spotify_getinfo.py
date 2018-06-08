#!/usr/bin/python
import os


def perfect_length(str):
    if len(str) > 23:
        return str[:23] + '…'
    else:
        return str


playing_song = os.popen(
    "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player string:Metadata 2> /dev/null | sed -n \'/title/{n;p}\' | cut -d \'\"\' -f 2|tr -d '\n'"
).read()
playing_artist = os.popen(
    "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player string:Metadata 2> /dev/null | sed -n \'/artist/{n;n;p}\' | cut -d \'\"\' -f 2|tr -d '\n'"
).read()
playing_album = os.popen(
    "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player string:Metadata 2> /dev/null | sed -n \'/album\"/{n;p}\' | cut -d \'\"\' -f 2| cut -d' ' -f-6 | tr -d '\n'"
).read()

if playing_song and playing_artist and playing_album:
    print(
        f"🎶 {perfect_length(playing_song)} by {perfect_length(playing_artist)} on {perfect_length(playing_album)} 🎶"
    )
