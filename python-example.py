#!/usr/bin/python

## Uses the PyYAML library.  Will read PSDF from stdin, write the
## found particles' representations to stderr, and the corresponding
## PSDF back to stdout.  

import yaml

class Particle(yaml.YAMLObject):
    """A basic particle class.

    Inherit from yaml.YAMLObject in order to get automatic creation
    and dumping.
    """

    yaml_tag = u'!Particle'

    def __repr__(self):
        return '%s(id=%r, m=%r, t=%r, r=%r, v=%r)'%(self.__class__.__name__,self.id, self.m, self.t, self.r, self.v)



if __name__ == "__main__":
    import sys
    for particle in yaml.load_all(sys.stdin):
        sys.stderr.write(str(particle)+'\n')
        yaml.dump(particle,sys.stdout,explicit_start=True)
