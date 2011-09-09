from particle import *
import heapq

def integrate(ps, dT, safety_factor):
    """Returns a new system of particles that corresponds to ps,
    advanced forward in time by dT.  The system will be in a different
    order than safety_factor is used to choose the timestep for each
    particle according to: dt = safety_factor*collision_timescale.
    The bodies is ps are unaffected by this operation."""

    ps = [p.copy() for p in ps]

    tstop = ps[0].t + dT

    for p in ps:
        p.init_for_integration(ps, safety_factor)

    psheap = [(p.tnext, p.id, p) for p in ps]

    heapq.heapify(psheap)

    while psheap[0][0] < tstop:
        tnext,id,current_p = heapq.heappop(psheap)
        current_p.hermite_step(ps, safety_factor, tstop)
        heapq.heappush(psheap, (current_p.tnext, current_p.id, current_p))

    # Final step
    for p in ps:
        p.hermite_step(ps, safety_factor, tstop)

    return ps
