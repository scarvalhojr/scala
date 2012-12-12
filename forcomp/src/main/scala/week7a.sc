object week7a {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  def wordOccurrences(w: Word): Occurrences = {
    val occ = w.toLowerCase.toList.groupBy (e => e)
    val counts: Occurrences = occ map {case (ch, list) => (ch, list.length)} toList
    val sorted = counts sortWith ((e1,e2) => e1._1 < e2._1)
    sorted
  }                                               //> wordOccurrences: (w: week7a.Word)week7a.Occurrences

  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences((s foldLeft "") (_ + _))      //> sentenceOccurrences: (s: week7a.Sentence)week7a.Occurrences

  val n = 7                                       //> n  : Int = 7
  val s = (1 to n) flatMap (i => (1 to i) map (j => (i,j)))
                                                  //> s  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (2,1)
                                                  //| , (2,2), (3,1), (3,2), (3,3), (4,1), (4,2), (4,3), (4,4), (5,1), (5,2), (5,3
                                                  //| ), (5,4), (5,5), (6,1), (6,2), (6,3), (6,4), (6,5), (6,6), (7,1), (7,2), (7,
                                                  //| 3), (7,4), (7,5), (7,6), (7,7))

  val w = "Scala"                                 //> w  : java.lang.String = Scala
  val g = w.toLowerCase.toList.groupBy (e => e)   //> g  : scala.collection.immutable.Map[Char,List[Char]] = Map(c -> List(c), a -
                                                  //| > List(a, a), s -> List(s), l -> List(l))
  val list = g map {case (ch, list) => (ch, list.length)} toList
                                                  //> list  : List[(Char, Int)] = List((c,1), (a,2), (s,1), (l,1))
  val occ = list sortWith ((e1, e2) => e1._1 < e2._1)
                                                  //> occ  : List[(Char, Int)] = List((a,2), (c,1), (l,1), (s,1))
  val sen: List[Word] = List("ate","tea","sea")   //> sen  : List[week7a.Word] = List(ate, tea, sea)
  val all: List[Char] = ((sen foldLeft "") (_ + _)).toList
                                                  //> all  : List[Char] = List(a, t, e, t, e, a, s, e, a)

  sen groupBy (wordOccurrences(_))                //> res0: scala.collection.immutable.Map[week7a.Occurrences,List[week7a.Word]] =
                                                  //|  Map(List((a,1), (e,1), (t,1)) -> List(ate, tea), List((a,1), (e,1), (s,1)) 
                                                  //| -> List(sea))
  val sen_occ = sentenceOccurrences(sen)          //> sen_occ  : week7a.Occurrences = List((a,3), (e,3), (s,1), (t,2))

  List() :: List(occ)                             //> res1: List[List[(Char, Int)]] = List(List(), List((a,2), (c,1), (l,1), (s,1)
                                                  //| ))

  val sen2: List[Word] = List("pear", "beer")     //> sen2  : List[week7a.Word] = List(pear, beer)
  val sen2_occ = sentenceOccurrences(sen2)        //> sen2_occ  : week7a.Occurrences = List((a,1), (b,1), (e,3), (p,1), (r,2))
  
  val list_occ = List(sen_occ, sen2_occ)          //> list_occ  : List[List[(Char, Int)]] = List(List((a,3), (e,3), (s,1), (t,2))
                                                  //| , List((a,1), (b,1), (e,3), (p,1), (r,2)))
  list_occ map (e => ('x',4) :: e)                //> res2: List[List[(Char, Int)]] = List(List((x,4), (a,3), (e,3), (s,1), (t,2)
                                                  //| ), List((x,4), (a,1), (b,1), (e,3), (p,1), (r,2)))

  val p: List[Occurrences] = (for (n <- 1 to 2) yield sen_occ) toList
                                                  //> p  : List[week7a.Occurrences] = List(List((a,3), (e,3), (s,1), (t,2)), List
                                                  //| ((a,3), (e,3), (s,1), (t,2)))
  
  ((for (n <- 1 to 2) yield list_occ map (e => ('x', n) :: e)) flatten) toList
                                                  //> res3: List[List[(Char, Int)]] = List(List((x,1), (a,3), (e,3), (s,1), (t,2)
                                                  //| ), List((x,1), (a,1), (b,1), (e,3), (p,1), (r,2)), List((x,2), (a,3), (e,3)
                                                  //| , (s,1), (t,2)), List((x,2), (a,1), (b,1), (e,3), (p,1), (r,2)))
  def combinations(occ: Occurrences): List[Occurrences] = occ match {
    case (ch: Char, occ: Int) :: rest => {
      val tmp = for (n <- 1 to occ) yield combinations(rest) map (e => (ch, n) :: e)
      combinations(rest) ::: ((tmp flatten) toList)
    }
    case _ => List(List())
  }                                               //> combinations: (occ: week7a.Occurrences)List[week7a.Occurrences]

  val sen3: List[Word] = List("aabb")             //> sen3  : List[week7a.Word] = List(aabb)
  val sen3_occ = sentenceOccurrences(sen3)        //> sen3_occ  : week7a.Occurrences = List((a,2), (b,2))
  combinations(sen3_occ)                          //> res4: List[week7a.Occurrences] = List(List(), List((b,1)), List((b,2)), Lis
                                                  //| t((a,1)), List((a,1), (b,1)), List((a,1), (b,2)), List((a,2)), List((a,2), 
                                                  //| (b,1)), List((a,2), (b,2)))
}