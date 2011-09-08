#!/usr/bin/python

from psdf import *
from psdf.particle import *
import yaml

if __name__ == '__main__':
    import sys
    
    particles = [p for p in yaml.load_all(sys.stdin)]

    std_particles = coordinates.to_standard_units(coordinates.to_com_frame(particles))

    yaml.dump_all(std_particles, stream=sys.stdout, explicit_start=True)
