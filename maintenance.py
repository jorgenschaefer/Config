#!/usr/bin/env python

import datetime
import errno
import os
import random
import subprocess
import time


def main():
    # Add a random delay to reduce the risk of colliding with git
    # access from other hosts.
    time.sleep(random.randint(0, 30))
    commit_pull_push(os.path.expanduser("~/Projects/Config/"))
    commit(os.path.expanduser("~/Documents/"))
    one_week = (datetime.datetime.now() -
                datetime.timedelta(days=7))
    archive_older(os.path.expanduser("~/"),
                  os.path.expanduser("~/Files/Stuff/"),
                  one_week)
    # Idea: If stuff in ~/Files/Stuff/ that is older than a month gets
    # bigger than 1M, create a nice tarball in ~/Files/Archive/Stuff/


def commit(repo):
    """Simply commit this git repository."""
    if not os.path.isdir(os.path.join(repo, ".git")):
        return
    os.chdir(repo)
    run("git", "add", "-A", "*")
    # This would error if no files were to be commited
    with open("/dev/null", "w") as devnull:
        subprocess.call(["git", "commit", "-q", "-a",
                         "-m", "Automatic commit"],
                        stdout=devnull)


def commit_pull_push(repo):
    """Auto-commit all changes in repo and merge with upstream.

    To merge with upstream, we first pull and rebase, then push the
    new repository.

    """
    if not os.path.isdir(os.path.join(repo, ".git")):
        return
    os.chdir(repo)
    commit(repo)
    run("git", "pull", "-q", "--rebase")
    run("git", "push", "-q")


def archive_older(src, dest, old):
    """Archive old files (but not directories) from src to dest.

    This also ignores dotfiles.

    """
    if not os.path.isdir(src):
        return
    if not os.path.isdir(dest):
        os.makedirs(dest)
    for filename in os.listdir(src):
        if filename.startswith("."):
            continue
        full = os.path.join(src, filename)
        if os.path.isdir(full):
            continue
        st = os.stat(full)
        mtime = datetime.datetime.fromtimestamp(st.st_mtime)
        if mtime < old:
            print "Archiving " + filename
            archive(full, os.path.join(dest, filename))


def archive(src, dest):
    """Move the file from src to dest, without clobbering.

    Files get an index added to avoid clobbering.

    """
    i = 0
    while True:
        name = make_name(dest, i)
        try:
            fd = os.open(name,
                         os.O_WRONLY |
                         os.O_CREAT |
                         os.O_EXCL)
        except OSError as err:
            if err.errno == errno.EEXIST:
                i = i + 1
                continue
            raise
        os.close(fd)
        os.rename(src, name)
        break


def make_name(fullname, i):
    if i == 0:
        return fullname
    prefix, ext = os.path.splitext(fullname)
    return "{prefix} ({i}){ext}".format(prefix=prefix,
                                        i=i,
                                        ext=ext)


def run(*args):
    with open("/dev/null", "w") as devnull:
        subprocess.check_call(args, stdout=devnull)


main()
