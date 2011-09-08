require 'yaml'

#
#  we use "!Paricle" tag, which is defined as private in YAML standard.
#  Apparently, current Ruby Yaml library seems to think it is 
#  global. So we define global tags here
class Particle
  def taguri
#    return 'x-private:Particle'
   return 'tag:yaml.org,2002:Particle'
  end
end
#
# Current parser accepts both "!Particle" and "!!Particle" 
#
YAML.add_private_type('Particle') do |type, val|
  YAML.object_maker(Particle, val)
end
YAML.add_builtin_type('Particle') do |type, val|
  YAML.object_maker(Particle, val)
end
