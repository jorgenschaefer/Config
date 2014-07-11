#!/usr/bin/python

import argparse
import apt
import collections


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--required", action="store_true", default=False,
                        help="Mark required packages as wanted")
    parser.add_argument("--important", action="store_true", default=False,
                        help="Mark important packages as wanted")
    parser.add_argument("--standard", action="store_true", default=False,
                        help="Mark standard packages as wanted")
    parser.add_argument("--provides", action="store_true", default=False,
                        help="Follow dependencies on packages in Provides")
    parser.add_argument("--recommends", action="store_true", default=False,
                        help="Also follow Recommends: relations")
    parser.add_argument("--suggests", action="store_true", default=False,
                        help="Also follow Suggests: relations")
    parser.add_argument("--file", metavar="FILE",
                        help="Read wanted packages from FILE")
    args = parser.parse_args()

    cache = Cache(recommends=args.recommends,
                  suggests=args.suggests)

    wanted = set()
    if args.required:
        wanted.update(cache.get_required_packages())
    if args.important:
        wanted.update(cache.get_important_packages())
    if args.standard:
        wanted.update(cache.get_standard_packages())
    if args.file:
        wanted.update(cache.read_packages(args.file))

    installed = set(cache.get_installed_packages())
    wanted = set(cache.dependency_tree(wanted))
    orphans = installed - wanted

    if args.provides:
        # Orphans might not be orphans if they Provide packages that are
        # Depended upon by other packages
        for pkg_name in list(orphans):
            privdes = cache.get_provides(pkg_name)
            if privdes & wanted:
                orphans -= set(cache.dependency_tree([pkg_name]))

    for pkg in sorted(orphans):
        print pkg


class Cache(apt.Cache):
    def __init__(self, recommends=False, suggests=True):
        super(Cache, self).__init__()
        self.recommends = recommends
        self.suggests = suggests

    def get_required_packages(self):
        return [pkg.name for pkg in self
                if pkg.candidate is not None and
                pkg.candidate.priority == 'required']

    def get_important_packages(self):
        return [pkg.name for pkg in self
                if pkg.candidate is not None and
                pkg.candidate.priority == 'important']

    def get_standard_packages(self):
        return [pkg.name for pkg in self
                if pkg.candidate is not None and
                pkg.candidate.priority == 'standard']

    def read_packages(self, filename):
        with open(filename) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith("#"):
                    continue
                if line not in self:
                    raise RuntimeError("Unknown package: {}".format(line))
                yield line

    def get_installed_packages(self):
        return [pkg.name for pkg in self
                if pkg.installed is not None]

    def dependency_tree(self, package_names):
        seen = set()
        agenda = collections.deque(package_names)
        while agenda:
            pkg_name = agenda.popleft()
            if pkg_name in seen:
                continue
            seen.add(pkg_name)

            if pkg_name not in self:
                continue
            pkg = self[pkg_name]

            if pkg.installed is None:
                continue
            agenda.extend(dep.name
                          for depgroup in pkg.installed.dependencies
                          for dep in depgroup)
            if self.recommends:
                agenda.extend(dep.name
                              for depgroup in pkg.installed.recommends
                              for dep in depgroup)
            if self.suggests:
                agenda.extend(dep.name
                              for depgroup in pkg.installed.suggests
                              for dep in depgroup)
        return seen

    def get_provides(self, pkg_name):
        pkg = self[pkg_name]
        return set(pkg.installed.provides)


if __name__ == '__main__':
    main()
