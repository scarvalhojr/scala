object session3 {
  def product(f: Int => Int)(a: Int, b: Int): Int = {
  	def loop(acc: Int, a: Int): Int = {
  		if (a > b) acc
  		else loop(acc * f(a), a + 1)
  	}
  	
  	loop(1, a)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  product((x: Int) => x)(1, 3)                    //> res0: Int = 6
  
  def factorial(n: Int): Int =
  	if (n == 0) 1
  	else product((x: Int) => x)(1, n)         //> factorial: (n: Int)Int
  	
  factorial(5)                                    //> res1: Int = 120
  
  def repeat(f: Int => Int, p: (Int, Int) => Int, acc_start: Int)(a: Int, b: Int): Int = {
  	def loop(acc: Int, a: Int): Int = {
  		if (a > b) acc
  		else loop(p(acc, f(a)), a + 1)
  	}
  	
  	loop(acc_start, a)
  }                                               //> repeat: (f: Int => Int, p: (Int, Int) => Int, acc_start: Int)(a: Int, b: Int
                                                  //| )Int
  
  def factorial2(n: Int): Int =
  	if (n == 0) 1
  	else repeat((x: Int) => x, (x: Int, y: Int) => x * y, 1)(1, n)
                                                  //> factorial2: (n: Int)Int
  
  factorial2(5)                                   //> res2: Int = 120
}