#!/bin/python3

import subprocess
import sys
import socket

# Prevent multiple instances of this script running simultaneously
# https://stackoverflow.com/a/7758075
# def get_lock(process_name):
#     # Without holding a reference to our socket somewhere it gets garbage
#     # collected when the function exits
#     get_lock._lock_socket = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)

#     try:
#         # The null byte (\0) means the socket is created 
#         # in the abstract namespace instead of being created 
#         # on the file system itself.
#         # Works only in Linux
#         get_lock._lock_socket.bind('\0' + process_name)
#     except socket.error:
#         print("already started")
#         sys.exit()

# get_lock('wofi_menu_script')

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
                            "--show=drun,dmenu",
                            "--insensitive",
                            "--prompt", "Enter a command...",
                            "--location", "top",
                            "--lines", "10",
                            "--no-actions"],
                           capture_output=True,
                           input=options.encode(),
                           check=True)

trimmed = selection.stdout.decode().strip()

if trimmed == "":
    sys.exit()

command = commands.get(trimmed)

if command:
    command()
