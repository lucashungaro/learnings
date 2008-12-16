require 'test/unit'

class FixnumTest < Test::Unit::TestCase
  
  def test_create_fixnum
    f = 100
    assert_equal(Fixnum, f.class)
  end
  
  def test_abs
    f=-10
    assert_equal(10, f.abs)
  end
end