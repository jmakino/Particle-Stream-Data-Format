#!/usr/local/bin/ruby
require "rubygems"
require "opengl"
require "glut"
require "mathn"
include Math

require "psdf.rb"
class Particle
  attr_reader :id, :x, :t
  def extraporate(t, scale)
#    p self
    dt = t - @t
    pred=[]
    @x.each_index{|k|
      pred[k]=  ((@a[k]*dt*0.5+@v[k])*dt+ x[k] )/scale
    }
    pred
  end
end


$diffuseMaterial = [0.5,0.5,0.5,1.0];
$Material = [[0.1,0.1,0.1,1.0],[0.0,1.0,1.0,1.0],[1.0,1.0,1.0,1.0]]
$color = [0.0,1.0,1.0]
$frame = 0
$inc = 1
$size=0.004
$scale = 1
$radius = 10
$theta= 0
$phi = 0
$n=0
$location = 0
display = Proc.new {
  scale = $scale
  GL.Clear(GL::COLOR_BUFFER_BIT | GL::DEPTH_BUFFER_BIT);
  GL.PushMatrix
  print "time= #{$time}, n=#{$n}, frame=#{$frame}\n"
  for j in 0..$n-1
    if $pa[j]
      GL.PushMatrix
      #    GL.Material(GL::FRONT, GL::AMBIENT, $Material[j]);
      GL.Color($color)
      GL.Rotate($theta, 0.0, 0.0, 1.0)
      GL.Rotate($phi, 1.0, 0.0, 0.0)
#      GL.Translate($pa[j].x[0]/scale,$pa[j].x[1]/scale, $pa[j].x[2]/scale)
      GL.Translate(*$pa[j].extraporate($time, scale))
      GLUT.SolidSphere($size/scale*$radius, 10, 6);
      GL.PopMatrix
    end
  end
  GL.PopMatrix
  GLUT.SwapBuffers();
  incabs = $inc > 0 ? $inc:-$inc
  incsign = $inc > 0 ? 1:-1
  incabs.times{
    $frame += incsign
    if $frame >= $a.size 
      $frame=0
    elsif $frame <= 0
      $frame = $a.size - 1
    end
    x= $a[$frame]
    $pa[x.id]=x
    $time = x.t
  }
}

def init
   mat_specular = [ 1.0, 1.0, 1.0, 1.0 ];
   light_position = [ 0.0, 30.0, 50.0, 0.0 ];

   GL.ClearColor(0.0, 0.0, 0.0, 0.0);
   GL.ShadeModel(GL::SMOOTH);
   GL.Enable(GL::DEPTH_TEST);
   GL.Light(GL::LIGHT0, GL::POSITION, light_position);
   GL.Enable(GL::LIGHTING);
   GL.Enable(GL::LIGHT0);

   GL.Material(GL::FRONT, GL::DIFFUSE, $diffuseMaterial);
#   GL.Material(GL::FRONT, GL::SPECULAR, mat_specular);
#   GL.Material(GL::FRONT, GL::SHININESS, 25.0);
#   GL.ColorMaterial(GL::FRONT, GL::DIFFUSE);
   GL.Enable(GL::COLOR_MATERIAL);
  GL.MatrixMode(GL::PROJECTION);
  GLU.Perspective(40.0, 1.0, 1.0,  10.0);
  GL.MatrixMode(GL::MODELVIEW);
  GLU.LookAt(0.0, 0.0, 5.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0)
  
end

def showstep(direction)
  if $runstate == 1
    GLUT.IdleFunc(nil) 
    $runstate = 0;
  end
  $inc = -$inc if $inc < 0
  $inc = 1  if $inc == 0
  $inc = -$inc if direction < 0
end

keyboard = Proc.new {|key, x, y|
  case key
  when ?h,?H
      $inc =0
   when ?a,?A
     if $inc >= 0
       $inc += 1
     else
       $inc -= 1
     end
   when ?d,?D
     if $inc > 0
       $inc -= 1
     else
       $inc += 1
     end
  when ?s
    $scale *= 1.2
  when ?S
    $scale /= 1.2
  when ?R
    $radius *= 1.2
  when ?r
    $radius /= 1.2
  when ?x
    $phi += 3
  when ?X
    $phi -= 3
  when ?y
    $theta += 3
  when ?Y
    $theta -= 3
  when ?>,?.
    showstep(1)
  when ?<,?,
    showstep(-1)
  when ?q,?Q,27
    exit(0);
  end
  GLUT.PostRedisplay() if $runstate == 0

}

reshape = Proc.new { |w, h|
   GL.Viewport(0, 0,  w,  h);
   GL.MatrixMode(GL::PROJECTION);
   GL.LoadIdentity();
   if (w <= h)
      GL.Ortho(-50.0, 50.0, -50.0*h/w,
         50.0*h/w, -1.0, 1.0);
   else
      GL.Ortho(-50.0*w/h,
         50.0*w/h, -50.0, 50.0, -1.0, 1.0);
   end
   GL.MatrixMode(GL::MODELVIEW);
}

$runstate = 0
mouse = Proc.new {|button, state, x, y|
  case button
  when GLUT::LEFT_BUTTON
    if (state == GLUT::DOWN) 
      GLUT.IdleFunc($reDisplay) 
      $runstate = 1
    end
  when GLUT::MIDDLE_BUTTON
    if (state == GLUT::DOWN) 
      GLUT.IdleFunc(nil) 
      $runstate = 0
    end
  when GLUT::RIGHT_BUTTON
    $inc = - $inc if (state == GLUT::DOWN)
  end
}

$reDisplay = Proc.new {
#  print "redisplay called"
   GLUT.PostRedisplay();
}


f = open("testin","r")
$a=[]
while  s = f.gets("--- " )
  obj = YAML.load(s) 
  $a.push obj if obj
end
$pa=[]
$a.reverse_each{|x|  $pa[x.id]=x }
#$pa.compact!

$time=0
$n = $pa.length
print "n=", $n, "\n"
GLUT.Init
GLUT.InitDisplayMode(GLUT::DOUBLE | GLUT::RGB | GLUT::DEPTH);
GLUT.InitWindowSize(700, 700); 
GLUT.InitWindowPosition(100, 0);
GLUT.CreateWindow($0);
init
#GLUT.ReshapeFunc(reshape);
GLUT.KeyboardFunc(keyboard);
GLUT.MouseFunc(mouse);
GLUT.DisplayFunc(display); 
GLUT.MainLoop();
