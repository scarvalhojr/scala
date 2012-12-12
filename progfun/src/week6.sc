object week6 {
  val n = 7                                       //> n  : Int = 7
  (1 until n)                                     //> res0: scala.collection.immutable.Range = Range(1, 2, 3, 4, 5, 6)
  (1 until n) map (i => (i, i * 2))               //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,2), (2,4)
                                                  //| , (3,6), (4,8), (5,10), (6,12))
  (1 until n) map (i =>
    (1 until i) map (j => (i, j)))                //> res2: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2)), V
                                                  //| ector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6,1)
                                                  //| , (6,2), (6,3), (6,4), (6,5)))
  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j)))                //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,1
                                                  //| ), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,
                                                  //| 3), (6,4), (6,5))
  def scalarProduct(xs: List[Double], ys: List[Double]): Double =
    (for ((x, y) <- xs zip ys) yield x * y) sum   //> scalarProduct: (xs: List[Double], ys: List[Double])Double

  val l1 = List(1.0, 3, 7)                        //> l1  : List[Double] = List(1.0, 3.0, 7.0)
  val l2 = List(2.0, 8, 3, 5)                     //> l2  : List[Double] = List(2.0, 8.0, 3.0, 5.0)
  val z = l1 zip l2                               //> z  : List[(Double, Double)] = List((1.0,2.0), (3.0,8.0), (7.0,3.0))
  scalarProduct(l1, l2)                           //> res4: Double = 47.0

  case class Book(title: String, authors: List[String])

  val books: Set[Book] = Set(
    Book(title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : Set[week6.Book] = Set(Book(Effective Java,List(Bloch, Joshua)), Boo
                                                  //| k(Effective Java 2,List(Bloch, Joshua)), Book(Java Puzzlers,List(Bloch, Josh
                                                  //| ua, Gafter, Neal)), Book(Programming in Scala,List(Odersky, Martin, Spoon, L
                                                  //| ex, Venners, Bill)))

  val multiple = for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1                                      //> multiple  : scala.collection.immutable.Set[String] = Set(Bloch, Joshua)

}