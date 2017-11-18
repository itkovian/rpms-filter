#
# Copyright 2017-2017 Ghent University
#
# This file is part of vsc-rpms,
# originally created by the HPC team of Ghent University (http://ugent.be/hpc/en),
# with support of Ghent University (http://ugent.be/hpc),
# the Flemish Supercomputer Centre (VSC) (https://www.vscentrum.be),
# the Flemish Research Foundation (FWO) (http://www.fwo.be/en)
# and the Department of Economy, Science and Innovation (EWI) (http://www.ewi-vlaanderen.be/en).
#
# https://github.ugent.be/hpcugent/vsc-rpms
#
# vsc-rpms is free software: you can redistribute it and/or modify
# it under the terms of the GNU Library General Public License as
# published by the Free Software Foundation, either version 2 of
# the License, or (at your option) any later version.
#
# vsc-rpms is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU Library General Public License
# along with vsc-rpms. If not, see <http://www.gnu.org/licenses/>.
#
"""Module which holds some generic functions for cleanuprepo."""
import csv
import logging
import os
import re
from vsc.utils import fancylogger

logger = fancylogger.getLogger()


def readlockedpf(lockedpf):
    """Read a csv file with locked packages in it. Return the packages."""
    lockedp = []
    with open(lockedpf, 'rb') as f:
        reader = csv.reader(f)
        for row in reader:
            lockedp.append(row[0])
    return lockedp


def readremovepf(removepf):
    """Read an output file from "managerepo" to a list."""
    with open(removepf) as f:
        lines = f.read().splitlines()

    # remove empty lines
    lines = filter(None, lines)
    return lines


def makewildcardregex(listp):
    """Take a list of strings and replace them by a tuple with
    - a prefix of the string before the first wildcard (*)
    - replace * in the string by a regex expression that matches valid RPM names.
    """
    newlist = []
    for package in listp:
        prefix = package.split('*')[0]
        newlist.append((prefix, package.replace("*", "[-a-zA-Z0-9_.]*")))

    return newlist


def sortedfilterlockedp(lockedp, removep):
    keepers = set()
    lockedp = sorted(makewildcardregex(lockedp), reverse=True)
    candidates = set(removep)

    removep = sorted([(os.path.basename(p), p) for p in removep], reverse=True)

    for (locked_prefix, locked_package_regex) in lockedp:
        logging.debug("Checking all packages matching %s", locked_package_regex)
        skipped = set()

        for (bpackage, package) in removep:
            if re.search(locked_package_regex, bpackage):
                keepers.add(package)
                logging.debug("--------> keeping %s as it matches with %s", package, locked_package_regex)
                continue

            logging.debug("----> Comparing %s", bpackage)
            if locked_prefix and bpackage < locked_prefix:
                logging.debug("--------> breaking on %s and %s", package, locked_prefix)
                break

            skipped.add(package)
        removep = [p for p in removep if p not in skipped]

    removals = [p for p in candidates - keepers]
    removals.sort()

    logging.debug("Keepers: %s", keepers)
    logging.debug("To remove: %s", removals)

    return removals


def cleanup(lockedpf, removepf):
    """
    Take 2 files containing locked packages and removeable packages.

    Return a list of removeable packages.
    """
    lockedp = readlockedpf(lockedpf)
    removep = readremovepf(removepf)

    removep = sortedfilterlockedp(lockedp, removep)

    for package in removep:
        print "%s" % package
