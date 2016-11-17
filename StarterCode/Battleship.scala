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
import java.lang.Thread

import scala.collection.mutable

object Battleship {
  class Ship(var name: String, var length: Int, var startPos: (String, String));

  def main(args: Array[String]) {
    while (true) {
      this.connectToServer
      if (this.socket != null)
        gameMain
    }
//    randomizeShips(List(2, 3, 3, 4, 5))
//    ships.map((x: Ship) => {
//      val i: (String, String) = x.startPos
//      println("<" + i._1 + " " + i._2 + ">")
//    })
//    placeShips("SHWR")
//    printBoard(this.mygrid)
  }

  val DIM = 8
  var DEBUG = false

  val rand = new scala.util.Random

  var API_KEY: String = "835619560"
  var GAME_SERVER: String = "battleshipgs.purduehackers.com"

  var letters: Array[Char] = Array[Char]('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')
  var grid: Array[Array[Int]] = Array.ofDim[Int](DIM,DIM)
  var mygrid: Array[Array[Int]] = Array.ofDim[Int](DIM,DIM)

  def rcToLetters(r: Int, c: Int): String = letters(r) + "" + String.valueOf(c)
  def lettersToRC(str: String): (Int, Int) = (letters.indexOf(str(0)), Integer.valueOf(str(1)) - '0')

  var destroyer = new Ship("destroyer", 2, ("A0", "A1")) // 2
  var submarine = new Ship("destroyer", 3, ("B0", "B2")) // 3
  var cruiser = new Ship("cruiser", 3, ("C0", "C2")) // 3
  var battleship = new Ship("battleship", 4, ("D0", "D3")) // 4
  var carrier = new Ship("carrier", 5, ("E0", "E4")) // 5
  var ships = List[Ship](destroyer, submarine, cruiser, battleship, carrier)
  var shipMap = Map(
    "destroyer" -> destroyer,
    "submarine" -> submarine,
    "cruiser" -> cruiser,
    "battleship" -> battleship,
    "carrier" -> carrier
  )

  def isValidPlacement(rs: (Int, Int), cs: (Int, Int)): Boolean = {
    (rs._1 to rs._2).foreach((r: Int) => {
      (cs._1 to cs._2).foreach((c: Int) => {
        if (mygrid(r)(c) != -1)
          return false
      })
    })
    true
  }

  def printBoard(g: Array[Array[Int]]): Unit = {
    g.foreach((row: Array[Int]) => {
      row.foreach((u: Int) => {
        if (u == -1)
          print(' ')
        else
          print('o')
      })
      println()
    })
  }

  def isValidPlacement(rs: String, cs: String): Boolean = isValidPlacement(lettersToRC(rs), lettersToRC(cs))

  def placeShipRandom(ship: Ship): Unit = {
    var placed: Boolean = false
    while (!placed) {
      var row = rand.nextInt(DIM)
      var col = rand.nextInt(DIM)
      placed = true
    }
  }

  def randomizeShips(lengths: List[Int]): Unit =  {
    ships.reverse.foreach((ship: Ship) => {
      placeShipRandom(ship)
    })
  }

  def placeShips(opponentID: String): Unit = {
    var i = 0
    for (i <- 0 until 8) {
      var j = 0
      for (j <- 0 until 8) {
        mygrid(i)(j) = -1
        grid(i)(j) = -1
      }
    }

    //randomizeShips(ships)
    ships.foreach((ship: Ship) => {
      var start: (Int, Int) = lettersToRC(ship.startPos._1)
      var end: (Int, Int) = lettersToRC(ship.startPos._2)
      (start._1 to end._1).foreach((r: Int) => {
        (start._2 to end._2).foreach((c: Int) => {
          mygrid(r)(c) = 1
        })
      })
    })
  }


  def makeMove(): Unit = {
    while (true) {
      var i = rand.nextInt(DIM)
      var j = rand.nextInt(DIM)
      if (DEBUG) println(i, j)
      if (grid(i)(j) == -1) {
        val wasHitSunkOrMiss: String = placeMove(rcToLetters(i, j))
        if (wasHitSunkOrMiss == "Hit" || wasHitSunkOrMiss == "Sunk")
          grid(i)(j) = 1
        else
          grid(i)(j) = 0
        return
      }
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
          this.out.print(destroyer.startPos._1)
          this.out.print(destroyer.startPos._2)
          out.flush
        }
        else if (data.contains("Submarine")) {
          this.out.print(submarine.startPos._1)
          this.out.print(submarine.startPos._2)
          out.flush
        }
        else if (data.contains("Cruiser")) {
          this.out.print(cruiser.startPos._1)
          this.out.print(cruiser.startPos._2)
          out.flush
        }
        else if (data.contains("Battleship")) {
          this.out.print(battleship.startPos._1)
          this.out.print(battleship.startPos._2)
          out.flush
        }
        else if (data.contains("Carrier")) {
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
