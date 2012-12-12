package streams

object Terrain {
  val level: String = """------
                        |--ST--
                        |--oo--
                        |--oo--
                        |------""".stripMargin    //> level  : String = ------
                                                  //| --ST--
                                                  //| --oo--
                                                  //| --oo--
                                                  //| ------
  
  val vec: Vector[Char] = Vector('a','b','c')     //> vec  : Vector[Char] = Vector(a, b, c)
  vec(0)                                          //> res0: Char = a
  vec.length                                      //> res1: Int = 3

  val n: List[(Char,Int)] = List(('a',1),('b',2),('c',3))
                                                  //> n  : List[(Char, Int)] = List((a,1), (b,2), (c,3))
  n filter (i => i._2 > 1)                        //> res2: List[(Char, Int)] = List((b,2), (c,3))

  def neighborsWithHistory(b: Int, history: List[Char]): Stream[(Int, List[Char])] = {
    val l = for {
      neighbor <- List((5,'r'),(7,'d'))
    } yield (neighbor._1, neighbor._2 :: history)
    l.toStream
  }                                               //> neighborsWithHistory: (b: Int, history: List[Char])Stream[(Int, List[Char])]
                                                  //| 
  
  val nh = neighborsWithHistory(1, List('l','u')) //> nh  : Stream[(Int, List[Char])] = Stream((5,List(r, l, u)), ?)
  nh.toSet                                        //> res3: scala.collection.immutable.Set[(Int, List[Char])] = Set((5,List(r, l, 
                                                  //| u)), (7,List(d, l, u)))

  val s = Set(1,2,3)                              //> s  : scala.collection.immutable.Set[Int] = Set(1, 2, 3)
  s contains 1                                    //> res4: Boolean = true
}