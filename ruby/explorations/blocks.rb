def map(array)
  new_array = []
  array.each do |element|
    new_array << yield(element)
  end
  new_array
end

def turn_into_strings(array)
  map(array) { |element| element.to_s }
end


turn_into_strings [1,2,3]

