#!/usr/bin/python

from psdf import *
import yaml
from psdf.particle import *
import sys

if __name__ == '__main__':

    ps = [p for p in yaml.load_all(sys.stdin)]

    ke = energy.kinetic_energy(ps)
    pe = energy.potential_energy(ps)

    print 'Total energy = %g (ke = %g, pe = %g)\n'%(ke+pe, ke, pe)
