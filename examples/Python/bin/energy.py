#!/usr/bin/python

from psdf import *
import yaml
from psdf.particle import *
import sys

if __name__ == '__main__':

    particles = [p for p in yaml.load_all(sys.stdin)]

    ke = sum([p.kinetic_energy() for p in particles])
    pe = 0.5*sum([sum([p.potential_energy(p2) for p2 in particles if not p == p2]) for p in particles])

    print 'Total energy = %g (ke = %g, pe = %g)\n'%(ke+pe, ke, pe)
