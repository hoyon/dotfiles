#!/bin/python3

import subprocess
import sys


def sway_cmd(cmd):
    return lambda: subprocess.run(["swaymsg", cmd], check=True)


def run_script(script_name):
    return lambda: subprocess.run([f"~/.config/sway/{script_name}"], shell=True, check=True)


def run_shell(args):
    return lambda: subprocess.run(args, check=True)


commands = {
    "Split horizontal": sway_cmd("split horizontal"),
    "Split vertical": sway_cmd("split vertical"),
    "Tabbed layout": sway_cmd("layout tabbed"),
    "Stacking layout": sway_cmd("layout stacking"),
    "Default layout": sway_cmd("layout default"),
    "Lock": run_script("swaylock-fancy.sh"),
    "Quick add task - Todoist": run_script("todoist-quick-add.sh"),
    "Switch to window": run_script("switch-window.sh"),
    "Dismiss notifications": run_shell(["makoctl", "dismiss", "--all"]),
    "Connect magic trackpad": run_script("trackpad_connect.sh"),
}

keys = list(commands.keys())

options = "\n".join(keys)

selection = subprocess.run(["wofi",
                            "--dmenu",
                            "--insensitive",
                            "--cache-file", "/dev/null",
                            "--prompt", "Enter a command...",
                            "--location", "top",
                            "--lines", "10"],
                           capture_output=True,
                           input=options.encode(),
                           check=True)

trimmed = selection.stdout.decode().strip()

if trimmed == "":
    sys.exit()

command = commands.get(trimmed)

if command:
    command()
else:
    message = f"Invalid command: {trimmed}"
    subprocess.run(["notify-send", "-t", "2000", message], check=True)
