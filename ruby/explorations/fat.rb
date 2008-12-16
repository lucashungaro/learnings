def fatorial(num)
  if num == 1
    return 1
  end
  
  num * fatorial(num-1)
  
end

puts fatorial(10)