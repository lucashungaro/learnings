# the 'spaceship' operator returns a value from the comparison

values = [[1, 2000], [5, 1000], [3, 1500], [1, 900]]

puts "Original array: #{values.inspect}"

sorted = values.sort { |a, b|
  result = a[0] <=> b[0]
  
  # when the elements are equal, the result is 0
  if result == 0 #trying the second element to sort out
    a[1] <=> b[1]
  else
    result
  end
}

puts "Sorted array: #{sorted.inspect}"