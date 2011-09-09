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
        self.r = r[:]
        self.v = v[:]

        # Some attributes useful for N-Body integration
        self.a = [0, 0, 0] # Acceleration
        self.j = [0, 0, 0] # Jerk
        
        self.tpred = t
        self.rpred = self.r[:]
        self.vpred = self.v[:]
        
        self.tnext = self.t

    def __cmp__(self, p):
        """We compare particles only by tnext (not position, velocity,
        etc).  This is useful to maintain a heap of particles during
        the integration of an N-body system."""
        if self.tnext < p.tnext:
            return -1
        elif self.tnext == p.tnext:
            return 0
        else:
            return 1

    def __hash__(self):
        """Hash only on tnext, to match comparison function."""
        return hash(self.tnext)
    
    def pred(self, t):
        """Predicts the position and velocity of the particle at the
        given time."""
        dt = t - self.t

        rpred = [r + dt*(v + (dt/2.0)*(a + (dt/3.0)*j))
                 for r,v,a,j in zip(self.r, self.v, self.a, self.j)]

        vpred = [v + dt*(a + (dt/2.0)*j)
                 for v,a,j in zip(self.v, self.a, self.j)]

        self.tpred = t
        self.rpred = rpred
        self.vpred = vpred

        return rpred, vpred

    def kinetic_energy(self):
        """Returns the kinetic energy of the particle."""
        return 0.5*self.m*dot(self.v, self.v)

    def potential_energy(self, p):
        """Returns the potential energy between this body and the
        given body."""
        d = distance(self.r, p.r)

        return -self.m*p.m/d

    def acc_and_jerk(self, p):
        """Returns the acceleration and jerk on this body due to
        particle p."""

        pr,pv = p.pred(self.t)

        r = [px - x for px,x in zip(pr, self.r)]
        v = [pv - v for pv,v in zip(pv, self.v)]

        rdv = dot(r,v)

        r2 = dot(r,r)
        r3 = r2*math.sqrt(r2)
        r5 = r3*r2

        acc = [p.m*self.m*r/r3 for r in r]
        jerk = [p.m*self.m*(v/r3 - 3.0*r*rdv/r5) for v,r in zip(v,r)]

        return acc,jerk

    def set_collision_time_scale(self, ps, safety_factor):
        """Sets the next time for this body to be its current time
        plus safety_factor*min_collision_time from among the ps."""
        t2 = 1e1000

        for p in ps:
            pr,pv = p.pred(self.t)
            
            r = [px - x for px,x in zip(pr, self.r)]
            v = [pv - v for pv,v in zip(pv, self.v)]

            r2 = dot(r,r)
            v2 = dot(v,v)

            pt2 = r2/v2

            t2 = min(t2, pt2)

        self.tnext = self.t + safety_factor*math.sqrt(t2)

    def hermite_step(self, ps, safety_factor):
        """Take one Hermite step in the system of bodies ps.  Safety
        factor is used to set the next time at the end of the step,
        via set_collision_time_scale."""

        totalacc = [0,0,0]
        totaljerk = [0,0,0]

        oldr = self.r
        oldv = self.v

        dt = self.tnext - self.t

        # Prediction step. 
        self.r, self.v = self.pred(self.tnext)
        self.t = self.tnext

        # Compute accumulated acc and jerk.
        for p in ps:
            acc,jerk = self.acc_and_jerk(p, self.t)
            totalacc = [ta + a for ta,a in zip(totalacc, acc)]
            totaljerk = [tj + j for tj,j in zip(totaljerk, jerk)]

        # Update step: new, corrected velocity and position
        self.v = [ov + (dt/2.0)*(ta+a + (dt/6.0)*(j - tj))
                  for ov,ta,a,j,tj in zip(oldv,totalacc,self.acc,self.jerk,totaljerk)]
        self.r = [r + (dt/2.0)*(ov + v + (dt/6.0)*(a - ta))
                  for r,ov,v,a,ta in zip(self.r, oldv, self.v, self.acc, totalacc)]

        # Store the computed acc and jerk at the new time
        self.acc = totalacc
        self.jerk = totaljerk
        self.t = tnext

        # Update next timestep.
        self.set_collision_time_scale(ps, safety_factor)
