object assignment {
  val word: List[Char] = List('a', 'b', 'f')      //> word  : List[Char] = List(a, b, f)
  val word2: List[Char] = List('y')               //> word2  : List[Char] = List(y)
	val counters : List[(Char, Int)] = List(('f',3),('d',8))
                                                  //> counters  : List[(Char, Int)] = List((f,3), (d,8))
	
	'x' :: word                               //> res0: List[Char] = List(x, a, b, f)
	word ::: word2                            //> res1: List[Char] = List(a, b, f, y)
	
  val sub = word match {
    case Nil => counters
    case head :: tail => {
      val num = counters find { case (c, _) => c == head } match {
        case Some((_, n)) => n + 1
        case None => 1
      }
      (head, num) :: (counters filter { case (c, _) => c != head })
    }
  }                                               //> sub  : List[(Char, Int)] = List((a,1), (f,3), (d,8))
  
  val sorted = sub sortWith ((e1,e2) => e1._1 < e2._1)
                                                  //> sorted  : List[(Char, Int)] = List((a,1), (d,8), (f,3))
  
  def times0(chars: List[Char], count: List[(Char, Int)]): List[(Char, Int)] =
    chars match {
      case Nil => count
      case head :: tail => {
        val num = count find { case (c, _) => c == head } match {
          case Some((_, n)) => n + 1
          case None => 1
        }
        times0(tail, (head, num) :: count filter { case (c, _) => c != head })
      }
    }                                             //> times0: (chars: List[Char], count: List[(Char, Int)])List[(Char, Int)]

  times0(word, List())                            //> res2: List[(Char, Int)] = List()
  
  type Bit = Int
  type CodeTable = List[(Char, List[Bit])]
  
  val table: CodeTable = List(('a',List(0)),('b',List(1)))
                                                  //> table  : assignment.CodeTable = List((a,List(0)), (b,List(1)))
  val table2: CodeTable = table map (code => (code._1, 0 :: code._2))
                                                  //> table2  : assignment.CodeTable = List((a,List(0, 0)), (b,List(0, 1)))
                                                  
  val less_list: List[Char] = List('a')           //> less_list  : List[Char] = List(a)
  val more_list: List[Char] = List('z')           //> more_list  : List[Char] = List(z)
  less_list ::: ('g' :: more_list)                //> res3: List[Char] = List(a, g, z)
  (less_list ::: List('h')) ::: more_list         //> res4: List[Char] = List(a, h, z)
}