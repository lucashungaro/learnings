require 'test/unit'
requite 'test_notifier'

class StringTest < Test::Unit::TestCase
  
   def test_length
      s = "Hello, World!"
      assert_equal(13,s.length)
   end

   def test_index
      s = "Rails"
      assert_equal(2, s.index("i")) 
      assert_equal(nil,s.index("x"))
   end

   #squeeze substitui caracteres repetidos por apenas uma ocorr�ncia do mesmo
   #� poss�vel estabelecer quais caracteres devem ser substitu�dos
   def test_squeeze
      s = "Hello"
      assert_equal("Helo", s.squeeze)

      s = "Ruby is goof proof"
      assert_equal("Ruby is gof prof", s.squeeze("a-z"))

      s = "ball shoot wall need"
      assert_equal("bal shot wal need", s.squeeze("l,o"))
   end

   def test_gsub
      s = "Este, texto, cont�m, muitas, v�rgulas"
      s = s.gsub(",", "")
      assert_equal("Este texto cont�m muitas v�rgulas", s)

      s = "Bola"
      s.gsub!(/[aeiou]/,"*") #altera o pr�prio objeto
      assert_equal("B*l*", s)
   end

   def test_regexp
      s = "17015-500"
      patt = /\d{5}[-]\d{3}/
      assert_match(patt, s)

      s = "(14) 3100-9999"
      patt = /[(]\d{2}[)]\s?\d{4}[-]\d{4}/
      assert_match(patt, s)

      s = "(14)3100-9999"
      assert_match(patt, s)
   end
  
end
