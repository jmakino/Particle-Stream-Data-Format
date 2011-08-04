require "psdf.rb"
class Particle
  attr_accessor :id, :x   
  def initialize
    @id=0
    @t=0
    @x=[0,1,2]
  end
end
(0..10).each{|id|
  obj=Particle.new
  obj.id =id;
  obj.x[0]=id*0.1
  print YAML.dump(obj)
}
