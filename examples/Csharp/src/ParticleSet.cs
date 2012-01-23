using System;
using System.Text;
using System.IO;
using System.Collections.Generic;
using YamlDotNet.RepresentationModel;

namespace PSDF
{
    public class ParticleSet
    {
        private List<Particle> particles;
        public List<Particle> Particles
        {
            get { return particles; }
        }

        public ParticleSet() {
            particles = new List<Particle>();
        }

        private String ReadString(YamlNode node) {
            return ((YamlScalarNode)node).Value;
        }

        private double ReadScalar(YamlNode node) {
            return double.Parse(ReadString(node));
        }

        private double ReadFromSequence(YamlNode node, int index) {
            return ReadScalar(((YamlSequenceNode)node).Children[index]);
        }

        public void Load(String filename) {
            StreamReader input = new StreamReader(filename);
            YamlStream yaml = new YamlStream();
            yaml.Load(input);

            foreach (YamlDocument doc in yaml.Documents) {
                Particle p = new Particle();
                YamlMappingNode mapping = (YamlMappingNode)doc.RootNode;
                foreach (KeyValuePair<YamlNode, YamlNode> entry in mapping.Children) {
                    string key = ((YamlScalarNode)entry.Key).Value;
                    if (key == "id") {
                        p.id = ReadString(entry.Value);
                    } else if (key == "m") {
                        p.m = ReadScalar(entry.Value);
                    } else if (key == "t") {
                        p.t = ReadScalar(entry.Value);
                    } else if (key == "t_max") {
                        p.t_max = ReadScalar(entry.Value);
                    } else if (key == "r") {
                        p.r = new double[3];
                        p.r[0] = ReadFromSequence(entry.Value, 0);
                        p.r[1] = ReadFromSequence(entry.Value, 1);
                        p.r[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "v") {
                        p.v = new double[3];
                        p.v[0] = ReadFromSequence(entry.Value, 0);
                        p.v[1] = ReadFromSequence(entry.Value, 1);
                        p.v[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "pot") {
                        p.pot = ReadScalar(entry.Value);
                    } else if (key == "acc") {
                        p.acc = new double[3];
                        p.acc[0] = ReadFromSequence(entry.Value, 0);
                        p.acc[1] = ReadFromSequence(entry.Value, 1);
                        p.acc[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "jerk") {
                        p.jerk = new double[3];
                        p.jerk[0] = ReadFromSequence(entry.Value, 0);
                        p.jerk[1] = ReadFromSequence(entry.Value, 1);
                        p.jerk[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "snap") {
                        p.snap = new double[3];
                        p.snap[0] = ReadFromSequence(entry.Value, 0);
                        p.snap[1] = ReadFromSequence(entry.Value, 1);
                        p.snap[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "crackle") {
                        p.crackle = new double[3];
                        p.crackle[0] = ReadFromSequence(entry.Value, 0);
                        p.crackle[1] = ReadFromSequence(entry.Value, 1);
                        p.crackle[2] = ReadFromSequence(entry.Value, 2);
                    } else if (key == "pop") {
                        p.pop = new double[3];
                        p.pop[0] = ReadFromSequence(entry.Value, 0);
                        p.pop[1] = ReadFromSequence(entry.Value, 1);
                        p.pop[2] = ReadFromSequence(entry.Value, 2);
                    }
                }
                particles.Add(p);
            }
        }

        public void Save(String filename) {
            using (StreamWriter outfile = new StreamWriter(filename)) {
                outfile.Write(ToString());
            }
        }

        public override String ToString() {
            StringBuilder result = new StringBuilder();
            foreach (Particle p in particles) {
                result.AppendLine("--- !Particle");
                if (p.id != null)
                    result.AppendLine("id: " + p.id);
                if (p.m != null)
                    result.AppendLine("m: " + p.m);
                if (p.t != null)
                    result.AppendLine("t: " + p.t);
                if (p.t_max != null)
                    result.AppendLine("t_max: " + p.t_max);
                if (p.r != null) {
                    result.AppendLine("r:");
                    result.AppendLine("  - " + p.r[0]);
                    result.AppendLine("  - " + p.r[1]);
                    result.AppendLine("  - " + p.r[2]);
                }
                if (p.v != null) {
                    result.AppendLine("v:");
                    result.AppendLine("  - " + p.v[0]);
                    result.AppendLine("  - " + p.v[1]);
                    result.AppendLine("  - " + p.v[2]);
                }
                if (p.pot != null)
                    result.AppendLine("pot: " + p.pot);
                if (p.acc != null) {
                    result.AppendLine("acc:");
                    result.AppendLine("  - " + p.acc[0]);
                    result.AppendLine("  - " + p.acc[1]);
                    result.AppendLine("  - " + p.acc[2]);
                }
                if (p.jerk != null) {
                    result.AppendLine("jerk:");
                    result.AppendLine("  - " + p.jerk[0]);
                    result.AppendLine("  - " + p.jerk[1]);
                    result.AppendLine("  - " + p.jerk[2]);
                }
                if (p.snap != null) {
                    result.AppendLine("snap:");
                    result.AppendLine("  - " + p.snap[0]);
                    result.AppendLine("  - " + p.snap[1]);
                    result.AppendLine("  - " + p.snap[2]);
                }
                if (p.crackle != null) {
                    result.AppendLine("crackle:");
                    result.AppendLine("  - " + p.crackle[0]);
                    result.AppendLine("  - " + p.crackle[1]);
                    result.AppendLine("  - " + p.crackle[2]);
                }
                if (p.pop != null) {
                    result.AppendLine("pop:");
                    result.AppendLine("  - " + p.pop[0]);
                    result.AppendLine("  - " + p.pop[1]);
                    result.AppendLine("  - " + p.pop[2]);
                }
            }
            return result.ToString();
        }
    }
}
