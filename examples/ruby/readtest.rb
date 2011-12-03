require "psdf.rb"

a = []
while s = gets("--- ")
  print s
  print "\n\nend of one gets\n\n"
  print s, "\n"
  obj = YAML.load(s) 
  a.push obj if obj
end
p a
