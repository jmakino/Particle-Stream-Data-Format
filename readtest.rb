require 'yaml'
class Particle
  def taguri
    return 'x-private:Particle'
  end
end
YAML.add_private_type('Particle') do |type, val|
  YAML.object_maker(Particle, val)
end
a = []
while s = gets("--- ")
  print s
  print "\n\nend of one gets\n\n"
#  s = "---" +s
  print s, "\n"
  obj = YAML.load(s) 
  a.push obj if obj
end
p a
