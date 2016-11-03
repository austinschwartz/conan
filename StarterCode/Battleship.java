/**
 * @ AUTHOR NAME HERE
 * @ Starter Code By Guocheng
 *
 * 2016-01-30
 * For: Purdue Hackers - Battleship
 * Battleship Client
 */

import java.io.*;
import java.util.*;
import java.net.Socket;
import java.net.InetAddress;
import java.lang.Thread;
import java.util.concurrent.ThreadLocalRandom;

public class Battleship {
	public static String API_KEY = System.getenv("BattleShipAPIKey");
	public static String GAME_SERVER = "battleshipgs.purduehackers.com";
  public static char[] letters = new char[] {'A','B','C','D','E','F','G','H'};
  public static int WIDTH = 8, HEIGHT = 8;
  public static enum State { UNKNOWN, MISS, HIT, SUNK, SUNKSHIP };

  List<Integer> remainingShips;
  List<int[]> mustExplore;
  State[][] grid;
  int[][] left, right, up, down;
  int[][] space, hits;

  Random rand = new Random();

  public String convertToBoardPos(int i, int j) {
    return this.letters[i] + String.valueOf(j);
  }

	void placeShips(String opponentID) {
    remainingShips = new ArrayList<>();
    mustExplore = new ArrayList<>();
    grid = new State[WIDTH][HEIGHT];
    left = new int[WIDTH][HEIGHT];
    right = new int[WIDTH][HEIGHT];
    up = new int[WIDTH][HEIGHT];
    down = new int[WIDTH][HEIGHT];
    space = new int[WIDTH][HEIGHT];
    hits = new int[WIDTH][HEIGHT];
    threePlaced = false;
    boolean[][] curr = new boolean[WIDTH][HEIGHT];
    for (int i = 0; i < WIDTH; i++)
      Arrays.fill(curr[i], false);
    int[] ships = new int[]{2, 3, 3, 4, 5};
    for (int ship : ships) {
      place(curr, ship);
      if (ship == 3)
        threePlaced = true;
    }
    curr = null;
	}
  public static enum Direction {DOWN, RIGHT};

  void place(boolean[][] curr, int len) {
    int i = -1, j = -1;
    int count = 0;
    Direction dir = Direction.RIGHT;
    while (true) {
      System.out.println(i + " " + j);
      i = ThreadLocalRandom.current().nextInt(0, 8);
      j = ThreadLocalRandom.current().nextInt(0, 8 - len);

      count = 0;
      dir = Direction.RIGHT;
      for (int k = 0; k < len; k++) {
        int nj = j + k;
        if (curr[i][nj] == true)
          break;
        curr[i][nj] = true;
        count += 1;
      }
      if (count == len) break; 

      i = ThreadLocalRandom.current().nextInt(0, 8 - len);
      j = ThreadLocalRandom.current().nextInt(0, 8);

      count = 0;
      dir = Direction.DOWN;
      for (int k = 0; k < len; k++) {
        int ni = i + k;
        if (curr[ni][j] == true)
          break;
        curr[ni][j] = true;
        count += 1;
      }
      if (count == len) break;
    }
    if (dir == Direction.RIGHT)
      placeShip(len, convertToBoardPos(i, j), convertToBoardPos(i, j + len - 1));
    else if (dir == Direction.DOWN)
      placeShip(len, convertToBoardPos(i, j), convertToBoardPos(i, j + len - 1));
  }

	void makeMove() {
		for(int i = 0; i < 8; i++) {
			for(int j = 0; j < 8; j++) {
				if (grid[i][j] == State.UNKNOWN) {
					switch (placeMove(i, j)) {
            case "Hit":
              grid[i][j] = State.HIT;
              break;
            case "Sunk":
              grid[i][j] = State.SUNK;
              break;
            case "Miss":
              grid[i][j] = State.MISS;
              break;
          }
					return;
				}
			}
		}
	}

	////////////////////////////////////// ^^^^^ PUT YOUR CODE ABOVE HERE ^^^^^ //////////////////////////////////////

	Socket socket;
	String[] destroyer = new String[] {"A0", "A0"};
	String[] submarine = new String[] {"A0", "A0"};
	String[] cruiser = new String[] {"A0", "A0"};
	String[] battleship = new String[] {"A0", "A0"};
	String[] carrier = new String[] {"A0", "A0"};

  boolean threePlaced = false;
	Boolean moveMade = false;
	String dataPassthrough;
	String data;
	BufferedReader br;
	PrintWriter out;

	public Battleship() {}

	void connectToServer() {
		try {
			InetAddress addr = InetAddress.getByName(GAME_SERVER);
      //InetAddress addr = InetAddress.getLocalHost();
			socket = new Socket(addr, 23345);
			br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
			out = new PrintWriter(socket.getOutputStream(), true);
      System.out.println(API_KEY);
			out.print(API_KEY);
			out.flush();
			data = br.readLine();
		} catch (Exception e) {
			System.out.println("Error: when connecting to the server..." + e.toString());
			socket = null; 
		}

		if (data == null || data.contains("False")) {
			socket = null;
			System.out.println("Invalid API_KEY");
			System.exit(1); // Close Client
		}
	}



	public void gameMain() {
		while (true) {
			try {
				if (this.dataPassthrough == null) {
					this.data = this.br.readLine();
				}
				else {
					this.data = this.dataPassthrough;
					this.dataPassthrough = null;
				}
			} catch (IOException ioe) {
				System.out.println("IOException: in gameMain"); 
				ioe.printStackTrace();
			}

			if (this.data == null) {
				try { this.socket.close(); } 
				catch (IOException e) { System.out.println("Socket Close Error"); }
				return;
			}

			if (data.contains("Welcome")) {
				String[] welcomeMsg = this.data.split(":");
				placeShips(welcomeMsg[1]);
				if (data.contains("Destroyer")) {
					this.dataPassthrough = "Destroyer(2):";
				}
			} else if (data.contains("Destroyer")) {
				this.out.print(destroyer[0]);
				this.out.print(destroyer[1]);
				out.flush();
			} else if (data.contains("Submarine")) {
				this.out.print(submarine[0]);
				this.out.print(submarine[1]);
				out.flush();
			} else if (data.contains("Cruiser")) {
				this.out.print(cruiser[0]);
				this.out.print(cruiser[1]);
				out.flush();
			} else if (data.contains("Battleship")) {
				this.out.print(battleship[0]);
				this.out.print(battleship[1]);
				out.flush();
			} else if (data.contains("Carrier")) {
				this.out.print(carrier[0]);
				this.out.print(carrier[1]);
				out.flush();
			} else if (data.contains( "Enter")) {
				this.moveMade = false;
				this.makeMove();
			} else if (data.contains("Error" )) {
				System.out.println("Error: " + data);
				System.exit(1); // Exit sys when there is an error
			} else if (data.contains("Die" )) {
				System.out.println("Error: Your client was disconnected using the Game Viewer.");
				System.exit(1); // Close Client
			} else {
				System.out.println("Recieved Unknown Response:" + data);
				System.exit(1); // Exit sys when there is an unknown response
			}
		}
	}

  void placeShip(int len, String startPos, String endPos) {
    System.out.println("Placing ship " + len + " at " + startPos + " " + endPos);
    switch (len) {
      case 2:
        destroyer = new String[] {startPos.toUpperCase(), endPos.toUpperCase()}; 
        break;
      case 3:
        if (!threePlaced) {
          submarine = new String[] {startPos.toUpperCase(), endPos.toUpperCase()}; 
          threePlaced = true;
        } else {
          cruiser = new String[] {startPos.toUpperCase(), endPos.toUpperCase()}; 
        }
        break;
      case 4:
        battleship = new String[] {startPos.toUpperCase(), endPos.toUpperCase()}; 
        break;
      case 5:
        carrier = new String[] {startPos.toUpperCase(), endPos.toUpperCase()}; 
        break;
    }
  }

	String placeMove(int i, int j) {
    String pos = convertToBoardPos(i, j);
		if (this.moveMade) { // Check if already made move this turn
			System.out.println("Error: Please Make Only 1 Move Per Turn.");
			System.exit(1); // Close Client
		}
		this.moveMade = true;

		this.out.print(pos);
		out.flush();
		try { data = this.br.readLine(); } 
		catch(Exception e) { System.out.println("No response after from the server after place the move"); }

		if (data.contains("Hit")) return "Hit";
		else if (data.contains("Sunk")) return "Sunk";
		else if (data.contains("Miss")) return "Miss";
		else {
			this.dataPassthrough = data;
			return "Miss";
		}
	}

	public static void main(String[] args) {
		Battleship bs = new Battleship();
		while(true) {
			bs.connectToServer();
			if (bs.socket != null) bs.gameMain();
		}	
	}
}

