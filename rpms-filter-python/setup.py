#!/usr/bin/env python
# -*- coding: latin-1 -*-
# #
# Copyright 2009-2017 Ghent University
#
# This file is part of vsc-rpms,
# originally created by the HPC team of Ghent University (http://ugent.be/hpc/en),
# with support of Ghent University (http://ugent.be/hpc),
# the Flemish Supercomputer Centre (VSC) (https://vscentrum.be/nl/en),
# the Hercules foundation (http://www.herculesstichting.be/in_English)
# and the Department of Economy, Science and Innovation (EWI) (http://www.ewi-vlaanderen.be/en).
#
# All rights reserved.
#
# #
"""Basic setup.py for vsc-rpms."""

import vsc.install.shared_setup as shared_setup
from vsc.install.shared_setup import wdp, ag

PACKAGE = {
    'version': '0.0.2',
    'author': [wdp, ag],
    'install_requires': [
        'vsc-utils >= 1.4.3',
        'vsc-install'
    ],
}


if __name__ == '__main__':
    shared_setup.action_target(PACKAGE)

