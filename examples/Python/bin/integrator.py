#!/usr/bin/python

import yaml
import sys
from psdf import *
from psdf.particle import *
import optparse

if __name__ == '__main__':
    parser=optparse.OptionParser()
    parser.add_option("-s", "--safety-factor", dest='sf',
                      help='fraction of collisional timescale for timestep', metavar='SF', action='store',
                      type='float', default=1e-2)
    parser.add_option('-t', '--delta-t', dest='dt',
                      help='amount to advance the system',
                      metavar='T', type='float',
                      action='store', default=1.0)

    (options,args) = parser.parse_args()

    ps = [p for p in yaml.load_all(sys.stdin)]

    integrator.integrate(ps, options.dt, options.sf, output=sys.stdout)
