object week7c {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  val dictionary: List[Word] = List("tea","eat","ate","bare","rabe","bear")
                                                  //> dictionary  : List[week7c.Word] = List(tea, eat, ate, bare, rabe, bear)
  def wordOccurrences(w: Word): Occurrences = {
    val ch_list = w.toLowerCase.toList.groupBy (e => e)
    val occ: Occurrences = (ch_list map {case (ch, list) => (ch, list.length)}).toList
    occ.sortWith(_._1 < _._1)
  }                                               //> wordOccurrences: (w: week7c.Word)week7c.Occurrences
  wordOccurrences("beer")                         //> res0: week7c.Occurrences = List((b,1), (e,2), (r,1))
  def sentenceOccurrences(s: Sentence): Occurrences =
    wordOccurrences((s foldLeft "") (_ + _))      //> sentenceOccurrences: (s: week7c.Sentence)week7c.Occurrences
  sentenceOccurrences(List("tate","beat","rate")) //> res1: week7c.Occurrences = List((a,3), (b,1), (e,3), (r,1), (t,4))
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    dictionary groupBy (wordOccurrences(_)) withDefaultValue List()
                                                  //> dictionaryByOccurrences  : Map[week7c.Occurrences,List[week7c.Word]] = <lazy
                                                  //| >
  dictionaryByOccurrences(wordOccurrences("abre"))//> res2: List[week7c.Word] = List(bare, rabe, bear)
  def combinations(occurrences: Occurrences): List[Occurrences] =
    occurrences match {
      case (ch, occ) :: rest => {
        val comb = for (n <- 1 to occ) yield combinations(rest) map (e => (ch, n) :: e)
        combinations(rest) ::: ((comb flatten) toList)
    }
    case _ => List(List())
  }                                               //> combinations: (occurrences: week7c.Occurrences)List[week7c.Occurrences]
  combinations(wordOccurrences("tea"))            //> res3: List[week7c.Occurrences] = List(List(), List((t,1)), List((e,1)), Lis
                                                  //| t((e,1), (t,1)), List((a,1)), List((a,1), (t,1)), List((a,1), (e,1)), List(
                                                  //| (a,1), (e,1), (t,1)))
  def subtract(x: Occurrences, y: Occurrences): Occurrences =  {
    def submap(x: Map[Char, Int], y: Occurrences): Map[Char, Int]= y match {
      case (ch, occ) :: rest => {
        val upd_occ = (x apply ch) - occ
        if (upd_occ > 0) submap(x updated (ch, upd_occ), rest)
        else submap(x - ch, rest)
      }
      case _ => x
    }
    (submap(x toMap, y).toList).sortWith(_._1 < _._1)
  }                                               //> subtract: (x: week7c.Occurrences, y: week7c.Occurrences)week7c.Occurrences
                                                  //| 
  subtract(wordOccurrences("tea"),wordOccurrences("ate"))
                                                  //> res4: week7c.Occurrences = List()
  subtract(wordOccurrences("tate"),wordOccurrences("at"))
                                                  //> res5: week7c.Occurrences = List((e,1), (t,1))
  def sentenceAnagrams(sentence: Sentence): List[Sentence] =
  {
    def anagrams(occ: Occurrences): List[Sentence] = {
      if (occ isEmpty) List(List())
      else {
        for {
          sub: Occurrences <- combinations(occ)
          if !(sub isEmpty)
          subsentences: Sentence <- anagrams(subtract(occ, sub))
          word: Word <- dictionaryByOccurrences.apply(sub)
        } yield word :: subsentences
      }
    }
    anagrams(sentenceOccurrences(sentence))
  }                                               //> sentenceAnagrams: (sentence: week7c.Sentence)List[week7c.Sentence]
  val sen = List("bear","ate")                    //> sen  : List[java.lang.String] = List(bear, ate)
  val sen_list = List(List("car"), List("bike"))  //> sen_list  : List[List[java.lang.String]] = List(List(car), List(bike))
  sen_list map (s => "new" :: s)                  //> res6: List[List[java.lang.String]] = List(List(new, car), List(new, bike))
                                                  //| 
  val oc = sentenceOccurrences(sen)               //> oc  : week7c.Occurrences = List((a,2), (b,1), (e,2), (r,1), (t,1))
  val cmb = combinations(oc)                      //> cmb  : List[week7c.Occurrences] = List(List(), List((t,1)), List((r,1)), Li
                                                  //| st((r,1), (t,1)), List((e,1)), List((e,1), (t,1)), List((e,1), (r,1)), List
                                                  //| ((e,1), (r,1), (t,1)), List((e,2)), List((e,2), (t,1)), List((e,2), (r,1)),
                                                  //|  List((e,2), (r,1), (t,1)), List((b,1)), List((b,1), (t,1)), List((b,1), (r
                                                  //| ,1)), List((b,1), (r,1), (t,1)), List((b,1), (e,1)), List((b,1), (e,1), (t,
                                                  //| 1)), List((b,1), (e,1), (r,1)), List((b,1), (e,1), (r,1), (t,1)), List((b,1
                                                  //| ), (e,2)), List((b,1), (e,2), (t,1)), List((b,1), (e,2), (r,1)), List((b,1)
                                                  //| , (e,2), (r,1), (t,1)), List((a,1)), List((a,1), (t,1)), List((a,1), (r,1))
                                                  //| , List((a,1), (r,1), (t,1)), List((a,1), (e,1)), List((a,1), (e,1), (t,1)),
                                                  //|  List((a,1), (e,1), (r,1)), List((a,1), (e,1), (r,1), (t,1)), List((a,1), (
                                                  //| e,2)), List((a,1), (e,2), (t,1)), List((a,1), (e,2), (r,1)), List((a,1), (e
                                                  //| ,2), (r,1), (t,1)), List((a,1), (b,1)), List((a,1), (b,1), (t,1)), List((a,
                                                  //| 1), (b,1), (r,1)), List
                                                  //| Output exceeds cutoff limit.
  sentenceAnagrams(sen)                           //> res7: List[week7c.Sentence] = List(List(tea, bare), List(eat, bare), List(a
                                                  //| te, bare), List(tea, rabe), List(eat, rabe), List(ate, rabe), List(tea, bea
                                                  //| r), List(eat, bear), List(ate, bear), List(bare, tea), List(rabe, tea), Lis
                                                  //| t(bear, tea), List(bare, eat), List(rabe, eat), List(bear, eat), List(bare,
                                                  //|  ate), List(rabe, ate), List(bear, ate))
  sentenceAnagrams(List("bear"))                  //> res8: List[week7c.Sentence] = List(List(bare), List(rabe), List(bear))
  for {
    sub: Occurrences <- combinations(oc)
    word: Word <- dictionaryByOccurrences.apply(sub)
  } yield (word, subtract(oc,sub))                //> res9: List[(week7c.Word, week7c.Occurrences)] = List((tea,List((a,1), (b,1)
                                                  //| , (e,1), (r,1))), (eat,List((a,1), (b,1), (e,1), (r,1))), (ate,List((a,1), 
                                                  //| (b,1), (e,1), (r,1))), (bare,List((a,1), (e,1), (t,1))), (rabe,List((a,1), 
                                                  //| (e,1), (t,1))), (bear,List((a,1), (e,1), (t,1))))
  (for {
    sub: Occurrences <- combinations(oc)
    word: Word <- dictionaryByOccurrences.apply(sub)
  } yield List(List("car"),List("red","bike")) map (s => word :: s)).flatten
                                                  //> res10: List[List[java.lang.String]] = List(List(tea, car), List(tea, red, b
                                                  //| ike), List(eat, car), List(eat, red, bike), List(ate, car), List(ate, red, 
                                                  //| bike), List(bare, car), List(bare, red, bike), List(rabe, car), List(rabe, 
                                                  //| red, bike), List(bear, car), List(bear, red, bike))
}