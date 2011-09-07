#!/usr/bin/python

import yaml

class Particle(yaml.YAMLObject):
    """A basic particle class.

    Inherit from yaml.YAMLObject in order to get automatic creation
    and dumping.
    """

    # '!!Particle' is shorthand for the following global tag:
    yaml_tag = u'tag:yaml.org,2002:Particle'

    def __repr__(self):
        return '%s(id=%r, m=%r, t=%r, r=%r, v=%r)'%(self.__class__.__name__,self.id, self.m, self.t, self.r, self.v)



if __name__ == "__main__":
    import sys
    for particle in yaml.load_all(sys.stdin):
        sys.stderr.write(str(particle)+'\n')
        yaml.dump(particle,sys.stdout,explicit_start=True)
