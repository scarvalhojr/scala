object WaterPouring {
  class Pouring(capacity: Vector[Int]) {

    type State = Vector[Int]
    val initialState = capacity map (x => 0)

    trait Move {
      def change(state: State): State
    }
    case class Empty(glass: Int) extends Move {
      def change(state: State) = state updated (glass, 0)
    }
    case class Fill(glass: Int) extends Move {
      def change(state: State) = state updated (glass, capacity(glass))
    }
    case class Pour(from: Int, to: Int) extends Move {
      def change(state: State) = {
        val amount = state(from) min (capacity(to) - state(to))
        state updated (from, state(from) - amount) updated (to, state(to) + amount)
      }
    }

    val glasses = 0 until capacity.length

    val moves =
      (for (g <- glasses) yield Empty(g)) ++
        (for (g <- glasses) yield Fill(g)) ++
        (for (from <- glasses; to <- glasses if from != to) yield Pour(from, to))

    class Path(history: List[Move], val endState: State) {
      // def endState: State = trackState(history)
      //private def trackState(xs: List[Move]): State = xs match {
      //  case Nil => initialState
      //  case move :: xs1 => move change trackState(xs1)
      //}
      //def endState: State = (history foldRight initialState)(_ change _)
      def extend(move: Move) = new Path(move :: history, move change endState)
      override def toString = (history.reverse mkString " ") + "--> " + endState
    }

    val initialPath = new Path(Nil, initialState)

    def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
      if (paths.isEmpty) Stream.empty
      else {
        val more = for {
          path <- paths
          next <- moves map path.extend
          if !(explored contains next.endState)
        } yield next
        paths #:: from(more, explored ++ (more map (_.endState)))
      }

    val pathSets = from(Set(initialPath), Set(initialState))

    def solutions(target: Int): Stream[Path] =
      for {
        pathSet <- pathSets
        path <- pathSet
        if path.endState contains target
      } yield path
  }

  val problem = new Pouring(Vector(4, 9, 19))     //> problem  : WaterPouring.Pouring = WaterPouring$Pouring@7825d2b2
  problem.glasses                                 //> res0: scala.collection.immutable.Range = Range(0, 1, 2)
  problem.initialState                            //> res1: scala.collection.immutable.Vector[Int] = Vector(0, 0, 0)
  problem.moves                                   //> res2: scala.collection.immutable.IndexedSeq[Product with Serializable with 
                                                  //| WaterPouring.problem.Move] = Vector(Empty(0), Empty(1), Empty(2), Fill(0), 
                                                  //| Fill(1), Fill(2), Pour(0,1), Pour(0,2), Pour(1,0), Pour(1,2), Pour(2,0), Po
                                                  //| ur(2,1))
  problem.initialPath                             //> res3: WaterPouring.problem.Path = --> Vector(0, 0, 0)
  problem.pathSets.take(2).toList                 //> res4: List[Set[WaterPouring.problem.Path]] = List(Set(--> Vector(0, 0, 0)),
                                                  //|  Set(Fill(0)--> Vector(4, 0, 0), Fill(1)--> Vector(0, 9, 0), Fill(2)--> Vec
                                                  //| tor(0, 0, 19)))
  problem.solutions(17)                           //> res5: Stream[WaterPouring.problem.Path] = Stream(Fill(1) Pour(1,2) Fill(0) 
                                                  //| Pour(0,2) Fill(0) Pour(0,2)--> Vector(0, 0, 17), ?)
}