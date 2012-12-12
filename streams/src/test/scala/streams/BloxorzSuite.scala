package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(terrain(Pos(4,9)), "4,9")
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(!terrain(Pos(5,10)), "5,10")
      assert(!terrain(Pos(6,9)), "6,9")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
      assert(goal == Pos(4,7))
    }
  }
  
  test("Move left Block((3,3),(3,4)) is standing") {
    new Level1 {
      val b = Block(Pos(3,3),Pos(3,4))
      val b2 = b.left
      assert(b2 === Block(Pos(3,2),Pos(3,2)))
      assert(b2.isStanding === true)
    }
  }
  
  test("startBlock is standing") {
    new Level1 {
      assert(startBlock isStanding)
    }
  }

  test("legalNeighbors of startBlock") {
    new Level1 {
      assert(startBlock.legalNeighbors === List((Block(Pos(1,2),Pos(1,3)),Right),
                                                (Block(Pos(2,1),Pos(3,1)),Down)))
    }
  }

  test("done?") {
    new Level1 {
      assert(done(Block(goal,goal)))
    }
  }

  test("neighborsWithHistory startBlock List()") {
    new Level1 {
      assert(neighborsWithHistory(startBlock, List()).toSet ===
             Set((Block(Pos(1,2),Pos(1,3)), List(Right)),
                 (Block(Pos(2,1),Pos(3,1)), List(Down))))
    }
  }

  test("newNeighborsOnly of neighborsWithHistory") {
    new Level1 {
      val neighbors = neighborsWithHistory(startBlock, List(Left,Up))
      assert(neighbors.toSet ===
             Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
                 (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
                 
      val explored = Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
      assert(newNeighborsOnly(neighbors, explored).toSet ===
             Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
