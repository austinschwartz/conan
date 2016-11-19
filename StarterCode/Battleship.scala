/**
  * @ AUTHOR NAME HERE
  * @ Starter Code By Guocheng
  *
  * 2016-01-30
  * For: Purdue Hackers - Battleship
  * Battleship Client
  */

import java.io._
import java.net.Socket
import java.net.InetAddress

import scala.collection.mutable
import scala.util.control.Breaks

object Battleship {
  class Ship(
    var name: String,
    var length: Int,
    var i: Int,
    var j: Int,
    var dir: Direction
  ) {
    var startPos = getPos(i, j, dir, length)

    def getPos(r: Int, c: Int, direc: Direction, len: Int): (String, String) = {
      flip(
        rcToLetters(r, c),
        direc match {
          case Right => rcToLetters(r, c + len - 1)
          case Left  => rcToLetters(r, c - len + 1)
          case Down  => rcToLetters(r + len - 1, c)
          case Up    => rcToLetters(r - len + 1, c)
          case _ => "A0"
        }
      )
    }

    def isValid(r: Int, c: Int, direc: Direction, len: Int): Boolean = {
      return r >= 0 && c >= 0 && r < DIM && c < DIM && (direc match {
        case Right => c + len - 1 < DIM
        case Left => c - len + 1 >= 0
        case Down => r + len - 1 < DIM
        case Up => r - len + 1 >= 0
      })
      false
    }
  }

  def main(args: Array[String]) {
    val fuckScala = new Breaks
    var flag = false
    if (!flag) {
      fuckScala.breakable {
        while (true) {
          try {
            this.connectToServer
            if (this.socket != null)
              gameMain
          } catch {
            case e: Exception => {
              println(e.toString())
              flag = true
              fuckScala.break()
            }
          }
        }
      }
    }
//    placeShips("SHWR")
//    ships.map((x: Ship) => {
//      val i: (String, String) = x.startPos
//      println("<" + i._1 + " " + i._2 + ">")
//    })
//    printBoard(this.mygrid)
  }

  sealed trait Direction
  final case object Left extends Direction
  final case object Right extends Direction
  final case object Up extends Direction
  final case object Down extends Direction
  var directions: List[Direction] = List(Left, Right, Up, Down)

  sealed trait State
  final case object Unknown extends State
  final case object Miss extends State
  final case object Hit extends State
  final case object Sunk extends State
  final case object SunkShip extends State
  var states: List[State] = List(Unknown, Miss, Hit, Sunk, SunkShip)

  val DIM = 8
  var DEBUG = true

  val rand = new scala.util.Random

  var API_KEY: String = "877036648"
  var GAME_SERVER: String = "battleshipgs.purduehackers.com"

  var letters: Array[Char] = Array[Char]('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
  var grid: Array[Array[Int]] = Array.ofDim[Int](DIM,DIM)
  var stateGrid: Array[Array[State]] = Array.ofDim[State](DIM,DIM)
  var remainingShips: List[Int] = List[Int]()
  var mygrid: Array[Array[Int]] = Array.ofDim[Int](DIM,DIM)
  var hitNext: mutable.Stack[(Int, Int)] = new mutable.Stack()

  def rcToLetters(r: Int, c: Int): String = letters(r) + "" + String.valueOf(c)
  def lettersToRC(str: String): (Int, Int) = (letters.indexOf(str(0)), Integer.valueOf(str(1)) - '0')

  var destroyer = new Ship("destroyer", 2, 0, 0, Right) // 2
  var submarine = new Ship("submarine", 3, 1, 0, Right) // 3
  var cruiser = new Ship("cruiser", 3, 2, 0, Right) // 3
  var battleship = new Ship("battleship", 4, 3, 0, Right) // 4
  var carrier = new Ship("carrier", 5, 4, 0, Right) // 5
  var ships = List[Ship](destroyer, submarine, cruiser, battleship, carrier)
  var shipMap = Map(
    "destroyer" -> destroyer,
    "submarine" -> submarine,
    "cruiser" -> cruiser,
    "battleship" -> battleship,
    "carrier" -> carrier
  )

  def flip(letters: (String, String)): (String, String) = {
    val (startX, startY) = lettersToRC(letters._1)
    val (endX,   endY)   = lettersToRC(letters._2)
    if (startX == endX) {
      if (endY < startY) {
        return (letters._2, letters._1)
      }
    } else if (startY == endY) {
      if (startX > endX) {
        return (letters._2, letters._1)
      }
    }
    return letters
  }

  def isValidPlacement(rs: String, cs: String): Boolean = isValidPlacement(lettersToRC(rs), lettersToRC(cs))
  def isValidPlacement(start: (Int, Int), end: (Int, Int)): Boolean = {
    var lowerR = Math.min(start._1, end._1)
    var higherR = Math.max(start._1, end._1)
    var lowerC = Math.min(start._2, end._2)
    var higherC = Math.max(start._2, end._2)
    (lowerR to higherR).foreach((r: Int) => {
      (lowerC to higherC).foreach((c: Int) => {
        if (mygrid(r)(c) != -1)
          return false
      })
    })
    return true
  }

  def printBoard(g: Array[Array[Int]]): Unit = {
    print(' ')
    letters.foreach((i: Char) => print(i))
    println()
    var num = 0
    g.foreach((row: Array[Int]) => {
      print(num)
      row.foreach((u: Int) => {
        if (u == -1)
          print(' ')
        else
          print('o')
      })
      println()
      num = num + 1
    })
  }

  def placeShipRandom(ship: Ship): Unit = {
    var placed: Boolean = false
    while (!placed) {
      val row = rand.nextInt(DIM)
      val col = rand.nextInt(DIM)
      var i = 0
      while (i < 6 && !placed) {
        val dir = directions(rand.nextInt(directions.length))

        if (ship.isValid(row, col, dir, ship.length) &&
          isValidPlacement(
            flip(ship.getPos(row, col, dir, ship.length))._1,
            flip(ship.getPos(row, col, dir, ship.length))._2
          )) {
          ship.i = row
          ship.j = col
          ship.startPos = flip(ship.getPos(row, col, dir, ship.length))
          ship.dir = dir
          placed = true
        }
        i = i + 1
      }
    }
  }

  def randomizeShips(): Unit =  {
    ships.reverse.foreach((ship: Ship) => {
      placeShipRandom(ship)
      updateMyGrid(ship)
    })
  }

  def placeShips(opponentID: String): Unit = {
    this.stateGrid = Array.ofDim[State](DIM,DIM)
    var i = 0
    var j = 0

    for (i <- 0 until DIM; j <- 0 until DIM) {
      this.stateGrid(i)(j) = Unknown
    }

    for (i <- 0 until 8; j <- 0 until 8) {
      mygrid(i)(j) = -1
      grid(i)(j) = -1
    }

    randomizeShips()
    printBoard(mygrid)
  }

  def updateMyGrid(ship: Ship): Unit = {
    var start: (Int, Int) = lettersToRC(ship.startPos._1)
    var end: (Int, Int) = lettersToRC(ship.startPos._2)
    println(ship.name, ship.length, ship.startPos, start, end)
    var lowerR = Math.min(start._1, end._1)
    var higherR = Math.max(start._1, end._1)
    var lowerC = Math.min(start._2, end._2)
    var higherC = Math.max(start._2, end._2)
    (lowerR to higherR).foreach((r: Int) => {
      (lowerC to higherC).foreach((c: Int) => {
        //println((r, c))
        mygrid(r)(c) = 1 // wtf?
      })
    })
  }
  def updateMyGrid(): Unit = {
    ships.foreach((ship: Ship) => {
      updateMyGrid(ship)
    })
  }

  def addAdjacenciesToStack(i: Int, j: Int): Unit = {
    def valid(i: Int, j: Int) = i >= 0 && j >= 0 && i < DIM && j < DIM
    if (valid(i + 1, j)) this.hitNext.push((i + 1, j))
    if (valid(i - 1, j)) this.hitNext.push((i - 1, j))
    if (valid(i, j + 1)) this.hitNext.push((i, j + 1))
    if (valid(i, j - 1)) this.hitNext.push((i, j - 1))
  }

  def getEvenParityMove(): (Int, Int) = {
    var x = 0
    while (x < 1000) {
      var i = rand.nextInt(DIM)
      var j = rand.nextInt(DIM)
      if ((i + j) % 2 != 0 && grid(i)(j) == -1) {
        return (i, j)
      }
      x += 1
    }
    getRandomMove()
  }

  def getRandomMove(): (Int, Int) = {
    while (true) {
      var i = rand.nextInt(DIM)
      var j = rand.nextInt(DIM)
      if (grid(i)(j) == -1) {
        return (i, j)
      }
    }
    (0, 0)
  }

  def getNextMove(): (Int, Int) = {
    while (this.hitNext.length > 0) {
      val move = this.hitNext.pop()
      if (grid(move._1)(move._2) == -1)
        return move
    }
    //getRandomMove()
    getEvenParityMove()
  }

  def makeMove(): Unit = {
    val (i, j) = getNextMove()
    if (DEBUG) println(i, j)
    val wasHitSunkOrMiss: String = placeMove(rcToLetters(i, j))
    if (wasHitSunkOrMiss == "Hit") {
      addAdjacenciesToStack(i, j)
      grid(i)(j) = 1
      stateGrid(i)(j) = Hit
    } else if (wasHitSunkOrMiss == "Sunk") {
      grid(i)(j) = 1
      stateGrid(i)(j) = Sunk
    } else {
      stateGrid(i)(j) = Miss
      grid(i)(j) = 0
    }
  }

  var socket: Socket = null
  var dataPassthrough: String = null
  var data: String = null
  var br: BufferedReader = null
  var out: PrintWriter = null
  var moveMade: Boolean = false

  def connectToServer: Unit = {
    try {
      val addr: InetAddress = InetAddress.getByName(this.GAME_SERVER)
      socket = new Socket(addr, 23345)
      br = new BufferedReader(new InputStreamReader(socket.getInputStream))
      out = new PrintWriter(socket.getOutputStream, true)
      out.print(this.API_KEY)
      out.flush
      data = br.readLine
    }
    catch {
      case e: Exception => {
        System.out.println("Error: when connecting to the server...")
        socket = null
      }
    }
    if (data == null || data.contains("False")) {
      socket = null
      System.out.println("Invalid API_KEY")
      System.exit(1)
    }
  }

  def gameMain: Unit = {
    while (true) {
      {
        try {
          if (this.dataPassthrough == null) {
            this.data = this.br.readLine
          }
          else {
            this.data = this.dataPassthrough
            this.dataPassthrough = null
          }
        }
        catch {
          case ioe: IOException => {
            System.out.println("IOException: in gameMain")
            ioe.printStackTrace
          }
        }
        if (this.data == null) {
          try {
            this.socket.close
          }
          catch {
            case e: IOException => {
              System.out.println("Socket Close Error")
            }
          }
          return
        }
        if (DEBUG)
          println(data)
        if (data.contains("Welcome")) {
          val welcomeMsg: Array[String] = this.data.split(":")
          placeShips(welcomeMsg(1))
          if (data.contains("Destroyer")) {
            this.dataPassthrough = "Destroyer(2):"
          }
        }
        else if (data.contains("Destroyer")) {
          println(destroyer.startPos)
          this.out.print(destroyer.startPos._1)
          this.out.print(destroyer.startPos._2)
          out.flush
        }
        else if (data.contains("Submarine")) {
          println(submarine.startPos)
          this.out.print(submarine.startPos._1)
          this.out.print(submarine.startPos._2)
          out.flush
        }
        else if (data.contains("Cruiser")) {
          println(cruiser.startPos)
          this.out.print(cruiser.startPos._1)
          this.out.print(cruiser.startPos._2)
          out.flush
        }
        else if (data.contains("Battleship")) {
          println(battleship.startPos)
          this.out.print(battleship.startPos._1)
          this.out.print(battleship.startPos._2)
          out.flush
        }
        else if (data.contains("Carrier")) {
          println(carrier.startPos)
          this.out.print(carrier.startPos._1)
          this.out.print(carrier.startPos._2)
          out.flush
        }
        else if (data.contains("Enter")) {
          this.moveMade = false
          this.makeMove
        }
        else if (data.contains("Error")) {
          System.out.println("Error: " + data)
          System.exit(1)
        }
        else if (data.contains("Die")) {
          System.out.println("Error: Your client was disconnected using the Game Viewer.")
          System.exit(1)
        }
        else {
          System.out.println("Received Unknown Response:" + data)
          System.exit(1)
        }
      }
    }
  }

  def placeMove(pos: String): String = {
    if (this.moveMade) {
      System.out.println("Error: Please Make Only 1 Move Per Turn.")
      System.exit(1)
    }
    this.moveMade = true
    this.out.print(pos)
    out.flush
    try {
      data = this.br.readLine
    }
    catch {
      case e: Exception => {
        System.out.println("No response after from the server after place the move")
      }
    }
    if (data.contains("Hit")) return "Hit"
    else if (data.contains("Sunk")) return "Sunk"
    else if (data.contains("Miss")) return "Miss"
    else {
      this.dataPassthrough = data
      return "Miss"
    }
  }
}
