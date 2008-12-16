class RecursiveChop
	def chop(numero, array)
		@min = @mid = 0
		
		@max = array.length
		@max -= 1
		
		self.rec_chop(numero, array)
	end
	

	def rec_chop(numero, arr_procura)
		
		if arr_procura.length == 0
			return -1
		end
		
		if arr_procura.length == 1
			if arr_procura[0] == numero
				return @min
			else
		  		return -1
			end
		end
		
		@mid = (arr_procura.length) / 2
		
		if numero == arr_procura[@mid]
			return @mid
		elsif numero < arr_procura[@mid]
			@max -= @mid
		else
			@min += @mid
		end
				
		rec_chop(numero, arr_procura[(@min..@max)])
	end
end

require 'test/unit'

class CodeKataTwo < Test::Unit::TestCase

	def test_chop

  		c = RecursiveChop.new
  
		assert_equal(-1, c.chop(3, []))
		assert_equal(-1, c.chop(3, [1]))
		assert_equal(0,  c.chop(1, [1]))
		#
		assert_equal(0,  c.chop(1, [1, 3, 5]))
		assert_equal(1,  c.chop(3, [1, 3, 5]))
		assert_equal(2,  c.chop(5, [1, 3, 5]))
		assert_equal(-1, c.chop(0, [1, 3, 5]))
		assert_equal(-1, c.chop(2, [1, 3, 5]))
		assert_equal(-1, c.chop(4, [1, 3, 5]))
		assert_equal(-1, c.chop(6, [1, 3, 5]))
		#
		assert_equal(0,  c.chop(1, [1, 3, 5, 7]))
		assert_equal(1,  c.chop(3, [1, 3, 5, 7]))
		assert_equal(2,  c.chop(5, [1, 3, 5, 7]))
		assert_equal(3,  c.chop(7, [1, 3, 5, 7]))
		assert_equal(-1, c.chop(0, [1, 3, 5, 7]))
		assert_equal(-1, c.chop(2, [1, 3, 5, 7]))
		assert_equal(-1, c.chop(4, [1, 3, 5, 7]))
		assert_equal(-1, c.chop(6, [1, 3, 5, 7]))
		assert_equal(-1, c.chop(8, [1, 3, 5, 7]))
	end
end

