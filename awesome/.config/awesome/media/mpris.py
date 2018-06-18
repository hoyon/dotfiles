import dbus
import re
import os
import sys
import fcntl
from enum import Enum
from dbus.mainloop.glib import DBusGMainLoop
from gi.repository import GLib
from functools import partial

DBusGMainLoop(set_as_default=True)

class IODriver(object):
    def __init__(self, line_callback):
        self.buffer = ''
        self.line_callback = line_callback
        flags = fcntl.fcntl(sys.stdin.fileno(), fcntl.F_GETFL)
        flags |= os.O_NONBLOCK
        fcntl.fcntl(sys.stdin.fileno(), fcntl.F_SETFL, flags)
        GLib.io_add_watch(sys.stdin, GLib.IO_IN, self.io_callback)

    def io_callback(self, fd, condition):
        chunk = fd.read()
        for char in chunk:
            self.buffer += char
            if char == '\n':
                self.line_callback(self.buffer)
                self.buffer = ''

        return True

def status2sym(status):
    if status == 'Playing':
        return ''
    elif status == 'Paused':
        return ''
    elif status == 'Stopped':
        return ''
    else:
        return '?'

class Player:
    def __init__(self, bus):
        self.bus = bus
        obj = session_bus.get_object(bus, '/org/mpris/MediaPlayer2')
        self.root_iface = dbus.Interface(obj, dbus_interface='org.mpris.MediaPlayer2')
        self.player_iface = dbus.Interface(obj, dbus_interface='org.mpris.MediaPlayer2.Player')
        self.prop_iface = dbus.Interface(obj, dbus_interface='org.freedesktop.DBus.Properties')
        self.name = str(self.get_root_prop('Identity'))
        self.status = str(self.get_player_prop('PlaybackStatus'))
        self.prop_iface.connect_to_signal('PropertiesChanged', self.prop_changed_cb)
        self.meta = self.get_player_prop('Metadata')

        # Methods
        # self.pause = dbus.Interface.get_dbus_method("PlayPause", self.player_iface)
        # self.next = dbus.Interface.get_dbus_method("Next", self.player_iface)
        # self.previous = dbus.Interface.get_dbus_method("Previous", self.player_iface)

    def get_root_prop(self, prop):
        return self.prop_iface.Get('org.mpris.MediaPlayer2', prop)

    def get_player_prop(self, prop):
        return self.prop_iface.Get('org.mpris.MediaPlayer2.Player', prop)

    def format_output(self):
        output = ''
        if 'xesam:artist' in self.meta:
            artists = self.meta['xesam:artist']
            output = '{} - '.format(artists[0])

        if 'xesam:title' in self.meta:
            output = output + self.meta['xesam:title']

        if self.status == 'Stopped':
            return ''

        status = status2sym(self.status)

        return '{} {}'.format(status, output)

    def print_output(self):
        if self.current:
            print(self.format_output())

    def prop_changed_cb(self, interfaced, changed, invalidated):
        if 'Metadata' in changed and self.meta != changed['Metadata']:
            self.meta = changed['Metadata']
            self.print_output()
        if 'PlaybackStatus' in changed:
            self.status = changed['PlaybackStatus']
            self.print_output()

    def pause(self):
        self.player_iface.PlayPause()

    def next(self):
        self.player_iface.Next()

    def previous(self):
        self.player_iface.Previous()


def is_mpris_bus(name):
    match = re.search(r'^org\.mpris\.MediaPlayer2\.*', name)
    return bool(match)

players = []

def select_current():
    if len(players) > 0:
        for player in players:
            player.current = False
        players[-1].current = True
        players[-1].print_output()
    else:
        print('')

def rotate():
    global players
    players = players[1:] + players[:1]

def name_changed_cb(name, old_owner, new_owner):
    if is_mpris_bus(name):
        if not old_owner:
            player = Player(name)
            players.append(player)

        elif not new_owner:
            for p in players:
                if p.bus == name:
                    players.remove(p)

        select_current()

def stdin_callback(line):
    line = line.strip()
    if len(players) > 0:
        if line == "pause":
            players[-1].pause()
        elif line == "next":
            players[-1].next()
        elif line == "previous":
            players[-1].previous()
        elif line == "rotate":
            rotate()
            select_current()
        elif line == "players":
            for player in players:
                print(player.name)
        else:
            print("なにこれ")

session_bus = dbus.SessionBus()

dbus_object = session_bus.get_object('org.freedesktop.DBus', '/')
dbus_iface = dbus.Interface(dbus_object, dbus_interface='org.freedesktop.DBus')
session_bus.add_signal_receiver(name_changed_cb,
                                dbus_interface='org.freedesktop.DBus',
                                signal_name='NameOwnerChanged')

for bus in dbus_iface.ListNames():
    if is_mpris_bus(bus):
        players.append(Player(bus))

select_current()

IODriver(stdin_callback)

loop = GLib.MainLoop()
loop.run()
