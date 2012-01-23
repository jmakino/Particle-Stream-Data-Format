using System;
using PSDF;

class PSDFTest
{
    static void Main(string[] args)
    {
        ParticleSet ps = new ParticleSet();
        ps.Load("test.psdf");

        Console.WriteLine(ps.Particles.Count + " particle(s) loaded:");
        Console.WriteLine(ps);

        Console.WriteLine("Saving particles to out.psdf");
        ps.Save("out.psdf");
    }
}
