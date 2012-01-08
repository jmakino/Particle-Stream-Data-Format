package com.github.jmakino;

public class Particle
{
    public String id;        // index (can be arbitrary text)
    public Double m;         // mass
    public Double t;         // time
    public Double t_max;     // max time to which this record is valid
    public Double[] r;       // position, array with three elements
    public Double[] v;       // velocity, array with three elements
    public Double pot;       // potential
    public Double[] acc;     // acceleration, array with three elements
    public Double[] jerk;    // jerk, array with three elements
    public Double[] snap;    // snap, array with three elements
    public Double[] crackle; // crackle, array with three elements
    public Double[] pop;     // pop, array with three elements

    public Particle()
    {
    }
}
