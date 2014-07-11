#!/usr/bin/env python

# Simple server cleanup script for Debian.

# Author: Jorgen Schaefer <contact@jorgenschaefer.de>
# License: GPL

# Takes a list of wanted packages (comments with # allowed), marks
# everything else as automatically installed, uses apt-get autoremove
# to clean up the packages, and then checks whether any installed
# package versions are not even available from any currently
# configured sources.

import os
import subprocess
import sys


def main():
    if len(sys.argv) <= 1:
        packagefile = os.path.expanduser("~/Files/packages.txt")
    else:
        packagefile = sys.argv[1]

    run("Orphaned Packages",
        ["deborphan.py", "--required", "--important", "--recommends",
         "--file", packagefile])
    run("Litter Packages",
        ["""dpkg -l | sed '1,5d' | grep -v ^ii | awk '{print $1 " " $2}'"""],
        shell=True)

    check_bad_origin()

    run("Partial Installs",
        ["dpkg", "--audit"])

    run("Deselected Packages",
        ["""dpkg --get-selections | grep -v '\tinstall$'"""],
        shell=True)

    run("Processes using deleted files",
        ["sudo checkrestart | grep -v 'Found 0 processes using "
         "old versions of upgraded files'"],
        shell=True)


def check_bad_origin():
    lines = []
    available = get_available_packages()
    for pkg, ver in get_installed_packages():
        if pkg not in available:
            lines.append("{0} installed: {1} available: None\n"
                         .format(pkg, ver))
        elif ver not in available[pkg]:
            lines.append("{0} installed: {1} available: {2}\n"
                         .format(pkg, ver, ", ".join(available[pkg])))

    if lines:
        sys.stdout.write("Uninstallable Packages\n"
                         "======================\n"
                         "\n")
        sys.stdout.write("".join(lines))
        sys.stdout.write("\n")


def get_available_packages():
    available = {}
    for filename in os.listdir("/var/lib/apt/lists/"):
        if filename.endswith("_Packages"):
            pkg = None
            ver = None
            with open(os.path.join("/var/lib/apt/lists/", filename)) as f:
                for line in f:
                    if line.startswith("Package:"):
                        _, pkg = line.strip().split(": ")
                        if ":" in pkg:
                            pkg, arch = pkg.split(":", 1)
                    elif line.startswith("Version:"):
                        _, ver = line.strip().split(": ")
                    elif line.strip() == '':
                        if pkg is not None and ver is not None:
                            available.setdefault(pkg, set())
                            available[pkg].add(ver)
    return available


def get_installed_packages():
    p = subprocess.Popen(["dpkg", "-l"], stdout=subprocess.PIPE)
    stdout, stderr = p.communicate()
    for line in stdout.split("\n"):
        if line.startswith("ii"):
            cols = line.split()
            pkg = cols[1]
            ver = cols[2]
            if ":" in pkg:
                pkg, arch = pkg.split(":", 1)
            yield pkg, ver


def run(header, *args, **kwargs):
    proc = subprocess.Popen(*args,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE,
                            stderr=subprocess.STDOUT,
                            **kwargs)
    output, ignored = proc.communicate()
    if output.strip():
        sys.stdout.write(header)
        sys.stdout.write("\n" + "=" * len(header) + "\n\n")
        sys.stdout.write(output)
        sys.stdout.write("\n")
    if proc.returncode == 0:
        return True
    else:
        return False


if __name__ == '__main__':
    main()
