from particle import *

def kinetic_energy(ps):
    """Returns the kinetic energy of the given system."""
    return sum([p.kinetic_energy() for p in ps])

def potential_energy(ps):
    """Returns the potential energy of the given system."""
    return 0.5*sum([sum([p.potential_energy(p2) for p2 in ps if p2 != p]) for p in ps])

def energy(ps):
    """Returns the total energy of the given system."""
    return kinetic_energy(ps) + potential_energy(ps)
