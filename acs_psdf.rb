#
# acs_psdf.rb
#
require "psdf.rb"
class Particle
  attr_accessor :id, :x, :v, :a, :p, :j, :m, :t, :dt
  def initialize
    @id=0
    @t=0
    @x=[0,1,2]
  end
end

class Body
  def to_psdf
    obj=Particle.new
    obj.id = @body_id
    obj.t  = @time
#    obj.dt = @new_time - @time
    obj.x = Array[*@pos]
    obj.v = Array[*@vel]
    obj.a = Array[*@acc]
    obj.j = Array[*@jerk]
    obj
  end
  def psdf_output
    print YAML.dump(self.to_psdf)
  end
end

