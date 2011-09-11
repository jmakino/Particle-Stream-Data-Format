import yaml
from vector import *
import math

class Particle(yaml.YAMLObject):
    """A particle class suitable for use with a Hermite integrator."""

    yaml_tag = u'!Particle'

    def __init__(self, idd, m, t, r, v):
        """Constructor.  Arguments are id, m, t, r, v."""
        self.id = idd
        self.m = m
        self.t = t
        self.r = Vector(r[:])
        self.v = Vector(v[:])

        # Some attributes useful for N-Body integration
        self.a = Vector([0, 0, 0]) # Acceleration
        self.j = Vector([0, 0, 0]) # Jerk
        
        self.tpred = t
        self.rpred = Vector(self.r[:])
        self.vpred = Vector(self.v[:])
        
        self.tnext = self.t

    # We need the following because we don't want to have our custom
    # Vector object appearing in the output; rather, all Vectors
    # should become straightforward YAML sequences.
    @classmethod
    def to_yaml(cls, dumper, data):
        """Dump to YAML."""
        return dumper.represent_mapping(data.yaml_tag,
                                        {'id': data.id,
                                         'm': data.m,
                                         't': data.t,
                                         'r': list(data.r),
                                         'v': list(data.v),
                                         'a': list(data.a),
                                         'j': list(data.a),
                                         'tpred': data.tpred,
                                         'rpred': list(data.rpred),
                                         'vpred': list(data.vpred),
                                         't_max': data.tnext})

    # Similarly to to_yaml, we need this routine to convert any
    # sequences in the input from the default Python lists to the
    # custom Vector class.
    @classmethod
    def from_yaml(cls, loader, node):
        """Load from YAML."""
        
        # Note the 'deep=True' argument.  This is apparently necessary
        # in order to get PyYAML to recursively convert all the
        # compound objects in node (i.e. the sequences for 'r:', 'v:',
        # etc.
        m = loader.construct_mapping(node, deep=True)

        p = Particle(m['id'], m['m'], m['t'], m['r'], m['v'])

        # The following fields may or may not be present.  If present,
        # we should also make sure they are vectors; if not present,
        # we should initialize them.
        if 'a' in m:
            p.a = Vector(m['a'])
        if 'j' in m:
            p.j = Vector(m['j'])
        if 'tpred' in m and 'rpred' in m and 'vpred' in m:
            p.tpred = m['tpred']
            p.rpred = Vector(m['rpred'])
            p.vpred = Vector(m['vpred'])
        if 't_max' in m:
            p.tnext = m['t_max']

        return p

    def __repr__(self):
        return 'Particle(id=%r, m=%r, t=%r, r=%r, v=%r)'%(self.id, self.m, self.t, self.r, self.v)

    def __str__(self):
        return 'Particle(id=%s, m=%s, t=%s, r=%s, v=%s)'%(self.id, self.m, self.t, self.r, self.v)

    def copy(self):
        """Returns a deep copy."""
        cop = Particle(self.id, self.m, self.t, self.r[:], self.v[:])

        cop.a = Vector(self.a[:])
        cop.j = Vector(self.j[:])

        cop.tpred = self.tpred
        cop.rpred = Vector(self.rpred[:])
        cop.vpred = Vector(self.vpred[:])

        cop.tnext = self.tnext

        return cop

    def pred(self, t):
        """Predicts the position and velocity of the particle at the
        given time."""
        dt = t - self.t

        rpred = self.r + dt*(self.v + (dt/2.0)*(self.a + (dt/3.0)*self.j))

        vpred = self.v + dt*(self.a + (dt/2.0)*self.j)

        self.tpred = t
        self.rpred = rpred
        self.vpred = vpred

        return rpred, vpred

    def kinetic_energy(self):
        """Returns the kinetic energy of the particle."""
        return 0.5*self.m*self.v*self.v

    def potential_energy(self, p):
        """Returns the potential energy between this body and the
        given body."""
        d = (self.r - p.r).norm()

        return -self.m*p.m/d

    def acc_and_jerk(self, p):
        """Returns the acceleration and jerk on this body due to
        particle p."""

        pr,pv = p.pred(self.t)

        r = pr - self.r
        v = pv - self.v

        rdv = r*v

        r2 = r*r
        r3 = r2*math.sqrt(r2)
        r5 = r3*r2

        acc = p.m*r/r3
        jerk = p.m*(v/r3 - 3.0*rdv*r/r5)

        return acc,jerk

    def total_acc_and_jerk(self, ps):
        """Returns the total acceleration and jerk on this object from
        the list of bodies ps.  Uses the t,r,v of self for
        computation."""

        totalacc = Vector([0,0,0])
        totaljerk = Vector([0,0,0])

        # Compute accumulated acc and jerk.
        for p in ps:
            if not p == self:
                acc,jerk = self.acc_and_jerk(p)
                totalacc += acc
                totaljerk += jerk

        return totalacc,totaljerk        

    def set_collision_time_scale(self, ps, safety_factor):
        """Sets the next time for this body to be its current time
        plus safety_factor*min_collision_time from among the ps."""
        t2 = 1e1000

        for p in ps:
            if p != self:
                pr,pv = p.pred(self.t)
            
                r = pr - self.r
                v = pv - self.v

                r2 = r*r
                v2 = v*v

                pt2 = r2/v2
                
                t2 = min(t2, pt2)

        self.tnext = self.t + safety_factor*math.sqrt(t2)

    def hermite_step(self, ps, safety_factor, tstop):
        """Take one Hermite step in the system of bodies ps.  Safety
        factor is used to set the next time at the end of the step,
        via set_collision_time_scale.  If the natural step would
        advance beyond tstop, then the step will be shortened to
        tstop."""

        oldr = self.r
        oldv = self.v

        if self.tnext > tstop:
            self.tnext = tstop

        dt = self.tnext - self.t

        # Prediction step. 
        self.r, self.v = self.pred(self.tnext)
        self.t = self.tnext

        newacc,newjerk=self.total_acc_and_jerk(ps)

        # Update step: new, corrected velocity and position
        self.v = oldv + (dt/2.0)*(newacc + self.a + (dt/6.0)*(self.j - newjerk))
        self.r = oldr + (dt/2.0)*(oldv + self.v + (dt/6.0)*(self.a - newacc))

        # Store the computed acc and jerk at the new time
        self.a = newacc
        self.j = newjerk

        # Update next timestep.
        self.set_collision_time_scale(ps, safety_factor)

        # ...and we're done.

    def init_for_integration(self, ps, safety_factor):
        """Should be called before any hermite integration steps to
        initialize the body for integration.  safety_factor is the
        fraction of the collision timescale used for computing the
        timestep."""

        self.a,self.j = self.total_acc_and_jerk(ps)

        # In principle, this call involves predicting the state of the
        # other bodies, but init_for_integration should only be called
        # at a synchronization point of the system, when all bodies
        # are at the same t.  So, it is safe to call here, even though
        # the other bodies may not have their acc and jerk computed
        # yet.
        self.set_collision_time_scale(ps, safety_factor)
