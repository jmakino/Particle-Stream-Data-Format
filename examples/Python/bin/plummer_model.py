#!/usr/bin/python

from psdf import *
from psdf.particle import *
import optparse
import yaml
import sys

# Output a plummer model.

if __name__ == "__main__":
    parser=optparse.OptionParser()
    parser.add_option("-n", "--number", dest="n",
                      help="create N bodies", metavar="N", action="store",
                      type="int")
    (options,args) = parser.parse_args()

    particles = []
    for i in range(options.n):
        r = ics.random_plummer_position()
        v = ics.random_plummer_velocity(r)
        particles.append(Particle(i, 1.0, 0.0, r, v))

    particles=coordinates.to_standard_units(coordinates.to_com_frame(particles))

    yaml.dump_all(particles, stream=sys.stdout)
