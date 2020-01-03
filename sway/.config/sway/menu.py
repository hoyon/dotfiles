#!/bin/python3

import subprocess


def sway_cmd(cmd):
    return lambda: subprocess.run(["swaymsg", cmd])


commands = {
    "Split horizontal": sway_cmd("split horizontal"),
    "Split vertical": sway_cmd("split vertical"),
    "Tabbed layout": sway_cmd("layout tabbed"),
    "Stacking layout": sway_cmd("layout stacking"),
    "Default layout": sway_cmd("layout default"),
    "Lock": lambda: subprocess.run(["swaylock"]),
}

keys = list(commands.keys())

options = "\n".join(keys)

selection = subprocess.run(["wofi",
                            "--dmenu",
                            "--insensitive",
                            "--cache-file", "/dev/null",
                            "--prompt", "Enter a command..."],
                           capture_output=True,
                           input=options.encode())

trimmed = selection.stdout.decode().strip()

if trimmed == "":
    exit()

command = commands.get(trimmed)

if command:
    command()
else:
    message = f"Invalid command: {trimmed}"
    subprocess.run(["notify-send", "-t", "2000", message])
