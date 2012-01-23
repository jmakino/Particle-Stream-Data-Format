using System;

namespace PSDF
{
    public class Particle
    {
        public String id;        // index (can be arbitrary text)
        public double? m;        // mass
        public double? t;        // time
        public double? t_max;    // max time to which this record is valid
        public double[] r;       // position, array with three elements
        public double[] v;       // velocity, array with three elements
        public double? pot;      // potential
        public double[] acc;     // acceleration, array with three elements
        public double[] jerk;    // jerk, array with three elements
        public double[] snap;    // snap, array with three elements
        public double[] crackle; // crackle, array with three elements
        public double[] pop;     // pop, array with three elements
    }
}
