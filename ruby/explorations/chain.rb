class Profile
  attr_accessor :name
end

class Model
  attr_accessor :profiles_list
  
  def initialize
    p = Profile.new
    p.name = 'profile 1'
    
    self.profiles_list = Array.new
    self.profiles_list << p    
  end
  
  def profiles
    #qualquer processamento
    
    return self
  end
  
  def add(profile)
    self.profiles_list << profile
  end
end

m = Model.new

puts "Listagem"
m.profiles_list.each do |p|
  puts p.name
end

p2 = Profile.new
p2.name = 'profile 2'

#encadeamento de metodos
m.profiles.add p2

puts "\nListagem"
m.profiles_list.each do |p|
  puts p.name
end