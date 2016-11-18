import scala.util.Random

/**
  * Created by schwar12 on 11/17/16.
  */
class Battleship$Test extends org.scalatest.FunSuite {
  test("converting r,c to letters works") {
    assert(Battleship.rcToLetters(0, 0) == "A0")
    assert(Battleship.rcToLetters(1, 0) == "B0")
    assert(Battleship.rcToLetters(2, 0) == "C0")
    assert(Battleship.rcToLetters(3, 0) == "D0")
    assert(Battleship.rcToLetters(7, 0) == "H0")
    assert(Battleship.rcToLetters(1, 1) == "B1")
    assert(Battleship.rcToLetters(1, 7) == "B7")
    assert(Battleship.rcToLetters(7, 7) == "H7")
  }

  test("converting letters to r,c works") {
    assert(Battleship.lettersToRC("A0") == (0, 0))
    assert(Battleship.lettersToRC("B0") == (1, 0))
    assert(Battleship.lettersToRC("C0") == (2, 0))
    assert(Battleship.lettersToRC("D0") == (3, 0))
    assert(Battleship.lettersToRC("H0") == (7, 0))
    assert(Battleship.lettersToRC("B1") == (1, 1))
    assert(Battleship.lettersToRC("B7") == (1, 7))
    assert(Battleship.lettersToRC("H7") == (7, 7))
  }

  test("ship is valid") {
    val ship = new Battleship.Ship("fuck", 2, 0, 0, Battleship.Right)
    assert(ship.isValid(0, 0, Battleship.Right, 2))
    assert(ship.isValid(0, 1, Battleship.Right, 2))
    assert(ship.isValid(0, 2, Battleship.Right, 2))
    assert(ship.isValid(0, 3, Battleship.Right, 2))
    assert(ship.isValid(0, 4, Battleship.Right, 2))
    assert(ship.isValid(0, 5, Battleship.Right, 2))
    assert(ship.isValid(0, 6, Battleship.Right, 2))
    assert(!ship.isValid(0, 7, Battleship.Right, 2))
    assert(ship.isValid(0, 7, Battleship.Down, 2))
    assert(!ship.isValid(0, 7, Battleship.Up, 2))
    assert(ship.isValid(1, 7, Battleship.Up, 2))
    assert(!ship.isValid(1, 7, Battleship.Up, 3))
    assert(ship.isValid(7, 7, Battleship.Up, 6))
    assert(ship.isValid(7, 7, Battleship.Left, 6))
    assert(ship.isValid(7, 7, Battleship.Right, 1))
    assert(ship.isValid(7, 7, Battleship.Down, 1))
    assert(!ship.isValid(7, 7, Battleship.Right, 2))
    assert(!ship.isValid(7, 7, Battleship.Down, 2))
    assert(ship.isValid(3, 3, Battleship.Down, 3))
    assert(ship.isValid(3, 3, Battleship.Up, 3))
    assert(ship.isValid(2, 7, Battleship.Down, 5))
  }

  test("flip works") {
    assert(Battleship.flip(("G3","G7")) == ("G3", "G7"))
    assert(Battleship.flip(("G7","G3")) == ("G3", "G7"))
    assert(Battleship.flip(("A7","B7")) == ("A7", "B7"))
    assert(Battleship.flip(("B7","A7")) == ("A7", "B7"))
    assert(Battleship.flip(("A0","A1")) == ("A0", "A1"))
    assert(Battleship.flip(("A1","A0")) == ("A0", "A1"))
  }

}
