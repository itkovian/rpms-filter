#!/usr/bin/env python
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
"""
Script that provides a list of rpms that are no longer needed.

It takes input from repomanage and vsc-quattor.
"""

import os
import sys
from vsc.utils import fancylogger
from vsc.utils.generaloption import simple_option
from vsc.rpms.rpmrepocleanup import cleanup

logger = fancylogger.getLogger()

def checkinput(options):
    """Validate input and return options."""
    if not options.locked_packages:
        logger.error( "locked_packages not given.")
        sys.exit(1)
    if not options.removeable_packages:
        logger.error("removeable_packages not given.")
        sys.exit(1)

    if not os.path.exists(options.removeable_packages):
        logger.error("%s does not exist.", options.removeable_packages)
        sys.exit(1)
    if not os.path.exists(options.locked_packages):
        logger.error("%s does not exist.", options.locked_packages)
        sys.exit(1)

    return options.locked_packages, options.removeable_packages

def main(options):
    """Main run of the script."""
    lockedpf, removepf = checkinput(options)
    cleanup(lockedpf, removepf)

if __name__ == '__main__':
    OPTIONS = {
        'locked_packages': ('A list of locked packages as provided by vsc-quattor.', None, 'store', None, 'l'),
        'removeable_packages': ('A list of removeable packages as provided by repomanage.', None, 'store', None, 'r'),
    }
    GO = simple_option(OPTIONS)
    logger.info("Starting main.")
    main(GO.options)
