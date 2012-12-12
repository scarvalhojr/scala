object week7d {
  type Word = String
  type Sentence = List[Word]
  type Occurrences = List[(Char, Int)]
  val dictionary: List[Word] = List("tea","eat","ate","bare","rabe","bear")
                                                  //> dictionary  : List[week7d.Word] = List(tea, eat, ate, bare, rabe, bear)
  def wordOccurrences(w: Word): Occurrences = {
    val ch_list = w.toLowerCase.toList.groupBy (e => e)
    val occ: Occurrences = (ch_list map {case (ch, list) => (ch, list.length)}).toList
    occ.sortWith(_._1 < _._1)
  }                                               //> wordOccurrences: (w: week7d.Word)week7d.Occurrences
  def slow_subtract(x: Occurrences, y: Occurrences): Occurrences =  {
    def subtractmap(x: Map[Char, Int], y: Occurrences): Map[Char, Int] =
    y match {
      case (ch, occ) :: rest => {
        val upd_occ = (x apply ch) - occ
        if (upd_occ > 0) subtractmap(x updated (ch, upd_occ), rest)
        else subtractmap(x - ch, rest)
      }
      case _ => x
    }
    (subtractmap(x toMap, y).toList).sortWith(_._1 < _._1)
  }                                               //> slow_subtract: (x: week7d.Occurrences, y: week7d.Occurrences)week7d.Occurren
                                                  //| ces
  def subtract(x: Occurrences, y: Occurrences): Occurrences =  {
    def subtractmap(x: Map[Char, Int], y: Occurrences): Map[Char, Int] = {
      y.foldLeft(x) ((m, occ) => {
        val upd_occ = (m apply occ._1) - occ._2
        if (upd_occ > 0) m updated (occ._1, upd_occ)
        else m - occ._1
      })
    }
    (subtractmap(x toMap, y).toList).sortWith(_._1 < _._1)
  }                                               //> subtract: (x: week7d.Occurrences, y: week7d.Occurrences)week7d.Occurrences
                                                  //| 
  subtract(wordOccurrences("tea"),wordOccurrences("ate"))
                                                  //> res0: week7d.Occurrences = List()
  subtract(wordOccurrences("tatety"),wordOccurrences("at"))
                                                  //> res1: week7d.Occurrences = List((e,1), (t,2), (y,1))
}