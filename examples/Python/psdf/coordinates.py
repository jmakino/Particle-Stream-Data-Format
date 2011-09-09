from particle import *
import energy

def total_mass(particles):
    return sum([p.m for p in particles])

def center_of_mass(particles):
    """Returns the center of mass of the given particles."""
    mtot = total_mass(particles)

    com = Vector([0,0,0])
    for p in particles:
        com += p.m*p.r/mtot

    return com
    
def total_momentum(particles):
    """Returns the total momentum of the given particles."""
    ptot = Vector([0, 0, 0])

    for p in particles:
        ptot += p.v*p.m

    return ptot

def to_com_frame(particles):
    """Returns a new array of particles with the same ID's, but in the
    center of momentum frame."""
    mtot = total_mass(particles)
    com = center_of_mass(particles)
    ptot = total_momentum(particles)

    vtot = ptot / mtot

    return [Particle(p.id, p.m, p.t, p.r - com, p.v - vtot) for p in particles]

def to_standard_units(particles):
    """Returns a new array of particles with the same ID's but now in
    standard units.

    The standard units are defined by the following:

    Mtot = 1.0
    G = 1.0
    Etot = -1/4

    where Mtot is the total mass, G is Newton's gravitational
    constant, and Etot is the total energy of the system.  See

    Heggie and Mathieu, Standardized Units and Time Scales in The Use
    of Supercomputers in Stellar Dynamics, McMillan and Hut, eds.,
    p. 233, 1986.
    """
    mtot = total_mass(particles)

    # Change masses first:
    particles = [Particle(p.id, p.m/mtot, p.t, p.r, p.v) for p in particles]

    # Now compute total energy
    ke = energy.kinetic_energy(particles)
    pe = energy.potential_energy(particles)
    e = ke + pe

    assert e < 0, e

    scalefactor = -0.25 / e

    return [Particle(p.id, p.m, p.t, p.r/scalefactor, p.v*math.sqrt(scalefactor)) for p in particles]
