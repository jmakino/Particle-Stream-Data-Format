require "psdf.rb"
class Particle
  attr_accessor :id, :r   
  def initialize
    @id=0
    @t=0
    @r=[0,1,2]
  end
end
(0..10).each{|id|
  obj=Particle.new
  obj.id =id;
  obj.r[0]=id*0.1
  print YAML.dump(obj)
}
