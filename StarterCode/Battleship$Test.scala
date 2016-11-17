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
}
