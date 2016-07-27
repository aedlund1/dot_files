#!/bin/python

import dbus

bus = dbus.SessionBus()
spotify = bus.get_object("org.mpris.MediaPlayer2.spotify", "/org/mpris/MediaPlayer2")
property_manager = dbus.Interface(spotify, "org.freedesktop.DBus.Properties")
props = property_manager.Get("org.mpris.MediaPlayer2.Player", "Metadata")
print("{} :: {}".format(" ".join(props['xesam:artist']), props['xesam:title']))
