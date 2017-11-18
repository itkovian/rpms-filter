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

import os
import shutil
import copy
from tempfile import mkdtemp
from vsc.install.testing import TestCase
from vsc.rpms.rpmrepocleanup import readlockedpf, readremovepf, sortedfilterlockedp, makewildcardregex


class TestRpmrepolocation(TestCase):
    """Test class for rpmrepolocation."""

    def setUp(self):
        """Set up temp dir."""
        self.tmpdir = mkdtemp()

    def tearDown(self):
        """clean up temp dir."""
        shutil.rmtree(self.tmpdir)

    def testreadlockedpf(self):
        """Test readlockedpf function."""

        # Valid input
        testfile = os.path.join(self.tmpdir, "lockedpackages.csv")
        with open(testfile, 'w') as fih:
            fih.write("kernel*,node1000,node1234\nblcr-21,muk102")
        self.assertEquals(readlockedpf(testfile), ['kernel*', 'blcr-21'])

        # Empty file
        with open(testfile, 'w') as fih:
            fih.write("\n")
        with self.assertRaises(IndexError):
            readlockedpf(testfile)

        # strange input
        with open(testfile, 'w') as fih:
            fih.write("!@#$%^&*()`~/\|,,")
        self.assertEquals(readlockedpf(testfile), ['!@#$%^&*()`~/\|'])

    def testreadremovepf(self):
        """Test readremovepf function."""
        testfile = os.path.join(self.tmpdir, "removeable_packages.txt")

        # Valid input
        with open(testfile, 'w') as fih:
            fih.write("/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm\n")
            fih.write("/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm\n")

        self.assertEquals(readremovepf(testfile), ['/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm',
                                                   '/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm'])

        # add an empty line
        with open(testfile, 'a') as fih:
            fih.write("\n\n\n/testrepo/icinga-1-1-1.rpm\n")

        self.assertEquals(readremovepf(testfile), ['/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm',
                                                   '/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm',
                                                   '/testrepo/icinga-1-1-1.rpm'])

    def testfilterlockedp(self):
        """Test filterlockedp function."""
        lockedp = ['kernel-firmware-2.6.32-431.29.2.el6',
                   'kernel*-3.10.0-327.28.3.el7',
                   'gpfs.gskit-8.0.50-*',
                   'Lmod-*.ug*']

        removep = ['/testrepo/torque-server-3.0.2-el6.ug.2.x86_64.rpm',
                   '/testrepo/torque-server-3.0.8-el6.ug.2.x86_64.rpm']

        expected = sorted(copy.deepcopy(removep))

        # Don't remove anything
        self.assertEquals(sortedfilterlockedp(lockedp, removep), expected)

        # Remove a hard locked version
        removep.append("/testrepo/kernel-firmware-2.6.32-431.29.2.el6.x86_64.rpm")
        self.assertEquals(sortedfilterlockedp(lockedp, removep), expected)

        # Remove a version with a * wildcard
        removep.append("/testrepo/kernel-firmware-3.10.0-327.28.3.el7.x86_64.rpm")
        self.assertEquals(sortedfilterlockedp(lockedp, removep), expected)

        # Remove a version with a * wildcard at the end
        removep.append("/test/repo/gpfs.gskit-8.0.50-888.noarch.rpm")
        self.assertEquals(sortedfilterlockedp(lockedp, removep), expected)

        # Remove a version with multiple * wildcards
        removep.append("/test/repo/Lmod-aalo.gskit-8.0.50.ug.-888.noarch.rpm")
        self.assertEquals(sortedfilterlockedp(lockedp, removep), expected)

    def testmakewildcardregex(self):
        """Test makewildcardregex function."""
        testlist = ['test*test.*.test', '*test*']
        self.assertEquals(makewildcardregex(testlist), [('test', 'test[-a-zA-Z0-9_.]*test.[-a-zA-Z0-9_.]*.test'),
                                                        ('', '[-a-zA-Z0-9_.]*test[-a-zA-Z0-9_.]*')])
