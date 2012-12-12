object session1_2 {
    def factorial(n: Int): Int = {
  	if (n == 0) 1 else n * factorial(n - 1)
  }                                               //> factorial: (n: Int)Int
  
  factorial(1000)                                 //> res0: Int = 0
  
  def factorial2(n: Int): Int = {
  	def loop(acc: Int, n: Int): Int =
  		if (n == 0) acc
  		else loop(acc * n, n - 1)
  	
  	loop(1, n)
  }                                               //> factorial2: (n: Int)Int
  
  factorial2(10000)                               //> res1: Int = 0
  
  def sum(f: Int => Int, a: Int, b: Int): Int = {
  	def loop(acc: Int, a: Int): Int = {
  		if (a > b) acc
  		else loop(acc + f(a), a + 1)
  	}
  	
  	loop(0, a)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  
  sum((x: Int) => x * x, 3, 5)                    //> res2: Int = 50

}