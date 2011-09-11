from particle import *
import heapq
import yaml

def integrate(ps, dT, safety_factor, output=None):
    """Returns a new system of particles that corresponds to ps,
    advanced forward in time by dT.  The system will be in a different
    order than safety_factor is used to choose the timestep for each
    particle according to: dt = safety_factor*collision_timescale.
    The bodies is ps are unaffected by this operation.  If output is
    given, it should be a stream, to which PSDF data is dumped at the
    start, during, and at the end of the integration."""

    ps = [p.copy() for p in ps]

    tstop = ps[0].t + dT

    for p in ps:
        p.init_for_integration(ps, safety_factor)

    if not output is None:
        yaml.dump_all(ps, explicit_start=True, stream=output)

    psheap = [(p.tnext, p.id, p) for p in ps]

    heapq.heapify(psheap)

    while psheap[0][0] < tstop:
        tnext,id,current_p = heapq.heappop(psheap)
        current_p.hermite_step(ps, safety_factor, tstop)
        if not output is None:
            yaml.dump(current_p, explicit_start=True, stream=output)
        heapq.heappush(psheap, (current_p.tnext, current_p.id, current_p))

    # Final step
    for p in ps:
        p.hermite_step(ps, safety_factor, tstop)

    if not output is None:
        yaml.dump_all(ps, explicit_start=True, stream=output)

    return ps
