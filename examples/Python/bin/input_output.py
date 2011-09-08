#!/usr/bin/python

import yaml
import psdf.particle

if __name__ == "__main__":
    import sys
    for particle in yaml.load_all(sys.stdin):
        sys.stderr.write(str(particle)+'\n')
        yaml.dump(particle,sys.stdout,explicit_start=True)
