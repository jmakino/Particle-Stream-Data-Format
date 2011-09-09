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

    particles=ics.plummer_system(options.n)

    yaml.dump_all(particles, stream=sys.stdout)
