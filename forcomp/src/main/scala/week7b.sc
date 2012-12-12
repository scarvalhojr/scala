object week7 {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  
  val dictionary: List[Word] = List("eat","tea","ate","pear","bear")
                                                  //> dictionary  : List[week7.Word] = List(eat, tea, ate, pear, bear)
  
  def wordOccurrences(w: Word): Occurrences = {
    val occ = w.toLowerCase.toList.groupBy (e => e)
    val counts: Occurrences = occ map {case (ch, list) => (ch, list.length)} toList
    val sorted = counts sortWith ((e1,e2) => e1._1 < e2._1)
    sorted
  }                                               //> wordOccurrences: (w: week7.Word)week7.Occurrences

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary groupBy (wordOccurrences(_)) withDefaultValue List()
                                                  //> dictionaryByOccurrences  : Map[week7.Occurrences,List[week7.Word]] = <lazy>
                                                  //| 

  val all = dictionaryByOccurrences.get(wordOccurrences("tea"))
                                                  //> all  : Option[List[week7.Word]] = Some(List(eat, tea, ate))

  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences match {
      case (ch: Char, occ: Int) :: rest => {
        val comb = for (n <- 1 to occ) yield combinations(rest) map (e => (ch, n) :: e)
        combinations(rest) ::: ((comb flatten) toList)
    }
    case _ => List(List())
  }                                               //> combinations: (occurrences: week7.Occurrences)List[week7.Occurrences]
  
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences((s foldLeft "") (_ + _))      //> sentenceOccurrences: (s: week7.Sentence)week7.Occurrences
  
  val anagrams = for {
    sub: Occurrences <- combinations(sentenceOccurrences(List("tea")))
    word: Word <- dictionaryByOccurrences.apply(sub)
  } yield (List(List("go","lucky"), List("gluck","yo")) map (s => word :: s))
                                                  //> anagrams  : List[List[List[java.lang.String]]] = List(List(List(eat, go, lu
                                                  //| cky), List(eat, gluck, yo)), List(List(tea, go, lucky), List(tea, gluck, yo
                                                  //| )), List(List(ate, go, lucky), List(ate, gluck, yo)))
  anagrams head                                   //> res0: List[List[java.lang.String]] = List(List(eat, go, lucky), List(eat, g
                                                  //| luck, yo))

  val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
                                                  //> x  : List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  val y = List(('r', 1))                          //> y  : List[(Char, Int)] = List((r,1))
  
  
  val k = x toMap                                 //> k  : scala.collection.immutable.Map[Char,Int] = Map(a -> 1, d -> 1, l -> 1,
                                                  //|  r -> 1)

  k updated ('a',42)                              //> res1: scala.collection.immutable.Map[Char,Int] = Map(a -> 42, d -> 1, l -> 
                                                  //| 1, r -> 1)
  k toList                                        //> res2: List[(Char, Int)] = List((a,1), (d,1), (l,1), (r,1))
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def submap(x: Map[Char, Int], y: Occurrences): Map[Char, Int]= y match {
      case (ch, occ) :: rest => {
        val curr_occ = k apply ch
        submap(x updated (ch, curr_occ - occ), rest)
      }
      case _ => x
    }
    submap(x toMap, y) toList
  }                                               //> subtract: (x: week7.Occurrences, y: week7.Occurrences)week7.Occurrences
}