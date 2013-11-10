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
import StringIO
import sys


def main():
    if len(sys.argv) <= 1:
        packagefile = os.path.expanduser("~/Files/packages.txt")
    else:
        packagefile = sys.argv[1]

    data = StringIO.StringIO()

    check_orphans(data, packagefile)
    check_litter(data)
    check_bad_origin(data)

    if os.isatty(sys.stdout.fileno()):
        less = subprocess.Popen(["less", "-F"], stdin=subprocess.PIPE)
        less.stdin.write(data.getvalue())
        less.stdin.close()
        less.communicate()
    else:
        sys.stdout.write(data.getvalue())


def check_orphans(stdout, packagefile):
    proc = subprocess.Popen(["deborphan", "-Ha", "--ignore-suggests",
                             "--no-show-section", "-k", packagefile],
                            stdout=subprocess.PIPE)
    data, ignored = proc.communicate()
    if proc.returncode != 0:
        exit("deborphan failed")
    if data.strip():
        stdout.write("Orphaned Packages\n"
                     "=================\n"
                     "\n")
        stdout.write(data)
        stdout.write("\n")


def check_litter(stdout):
    proc = subprocess.Popen(
        ["""dpkg -l | sed '1,5d' | grep -v ^ii | awk '{print $1 " " $2}'"""],
        shell=True,
        stdout=subprocess.PIPE)
    data, ignored = proc.communicate()
    if proc.returncode != 0:
        exit("dpkg failed")
    if data.strip():
        stdout.write("Litter Packages\n"
                     "===============\n"
                     "\n")
        stdout.write(data)
        stdout.write("\n")


def check_bad_origin(stdout):
    lines = []
    available = get_available_packages()
    for pkg, ver in get_installed_packages():
        if pkg not in available:
            lines.append("{0} version {1} installed, but not available\n"
                         .format(pkg, ver))
        elif ver not in available[pkg]:
            lines.append("{0} version {1} installed, but only {2} available\n"
                         .format(pkg, ver, ", ".join(available[pkg])))

    if lines:
        stdout.write("Uninstallable Packages\n"
                     "======================\n"
                     "\n")
        stdout.write("".join(lines))
        stdout.write("\n")


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


if __name__ == '__main__':
    main()
