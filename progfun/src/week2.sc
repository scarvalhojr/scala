object week2 {
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

  def product(f: Int => Int, a: Int, b: Int): Int = {
  	def loop(acc: Int, a: Int): Int = {
  		if (a > b) acc
  		else loop(acc * f(a), a + 1)
  	}
  	
  	loop(1, a)
  }                                               //> product: (f: Int => Int, a: Int, b: Int)Int
  
  product((x: Int) => x, 1, 3)                    //> res3: Int = 6
  
  def factorial3(n: Int): Int =
  	if (n == 0) 1
  	else product((x: Int) => x, 1, n)         //> factorial3: (n: Int)Int
  	
  factorial3(5)                                   //> res4: Int = 120
  
  def repeat(f: Int => Int, p: (Int, Int) => Int, acc_start: Int, a: Int, b: Int): Int = {
  	def loop(acc: Int, a: Int): Int = {
  		if (a > b) acc
  		else loop(p(acc, f(a)), a + 1)
  	}
  	
  	loop(acc_start, a)
  }                                               //> repeat: (f: Int => Int, p: (Int, Int) => Int, acc_start: Int, a: Int, b: In
                                                  //| t)Int
  
  def factorial4(n: Int): Int =
  	if (n == 0) 1
  	else repeat((x: Int) => x, (x: Int, y: Int) => x * y, 1, 1, n)
                                                  //> factorial4: (n: Int)Int
  
  factorial4(5)                                   //> res5: Int = 120
}