require 'yaml'
class Particle
  def taguri
    return 'x-private:Particle'
  end
end
YAML.add_private_type('Particle') do |type, val|
  YAML.object_maker(Particle, val)
end
