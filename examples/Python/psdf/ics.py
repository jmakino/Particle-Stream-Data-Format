# Useful functions for computing initial conditions

import math
import random
import vector
import coordinates
from particle import *

def uniform_vector_on_shell(r):
    """Returns a vector that is uniformly distributed over the
    spherical shell of radius r."""
    phi = random.uniform(0.0, 2.0*math.pi)
    cos_theta = random.uniform(-1.0, 1.0)
    sin_theta = math.sqrt(1.0-cos_theta*cos_theta)

    return Vector([r*math.cos(phi)*sin_theta,
                   r*math.sin(phi)*sin_theta,
                   r*cos_theta])
    

def random_uniform_position():
    """Returns a random position in the unit sphere."""
    r = math.pow(random.random(), 1.0/3.0)

    return uniform_vector_on_shell(r)

def random_plummer_position():
    """Returns a random position drawn from a plummer model."""
    r = 1.0 / math.sqrt(math.pow(random.random(), -2.0/3.0) - 1.0)

    return uniform_vector_on_shell(r)

def random_plummer_velocity(pos):
    """Returns a random plummer velocity, appropriate for a particle at 3-vector pos."""
    r = pos.norm()

    x = 0.0
    y = 0.1
    while y > x*x*math.pow((1.0-x*x), 3.5):
        x = random.random()
        y = random.uniform(0.0, 0.1)

    vmag = x * math.sqrt(2.0) * math.pow(1.0 + r*r, -0.25)

    return uniform_vector_on_shell(vmag)

def plummer_system(n):
    """Returns a system of bodies of length n distributed according to
    a Plummer model.  The returned system will be in standard
    units."""
    ps = []

    for i in range(n):
        r = random_plummer_position()
        v = random_plummer_velocity(r)
        ps.append(Particle(i, 1.0/n, 0.0, r, v))

    return coordinates.to_standard_units(coordinates.to_com_frame(ps))
