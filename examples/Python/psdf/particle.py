import yaml
from vector import *
import math

class Particle(yaml.YAMLObject):
    """A basic particle class.

    Inherit from yaml.YAMLObject in order to get automatic creation
    and dumping.
    """

    yaml_tag = u'!Particle'

    def __init__(self, idd, m, t, r, v):
        """Constructor.  Arguments are id, m, t, r, v."""
        self.id = idd
        self.m = m
        self.t = t
        self.r = r
        self.v = v

        # Some attributes useful for N-Body integration
        self.a = [0, 0, 0] # Acceleration
        self.j = [0, 0, 0] # Jerk
    
    def pred(self, t):
        """Predicts the position and velocity of the particle at the
        given time."""
        dt = t - self.t

        rpred = [r + dt*(v + (dt/2.0)*(a + (dt/3.0)*j))
                 for r,v,a,j in zip(self.r, self.v, self.a, self.j)]

        vpred = [v + dt*(a + (dt/2.0)*j)
                 for v,a,j in zip(self.v, self.a, self.j)]

        return rpred, vpred

    def kinetic_energy(self):
        """Returns the kinetic energy of the particle."""
        return 0.5*self.m*dot(self.v, self.v)

    def potential_energy(self, p):
        """Returns the potential energy between this body and the
        given body."""
        d = distance(self.r, p.r)

        return -self.m*p.m/d
