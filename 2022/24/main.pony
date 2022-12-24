use "files"
use "collections"
use "itertools"

class Coord
  let x: I32
  let y: I32

  new create(x': I32, y': I32) =>
    x = x'
    y = y'

  fun hash(): USize val =>
    (y.usize() << 32) or (x.usize())

  fun eq(that: Coord box): Bool =>
    (x == that.x) and (y == that.y)

  fun ne(that: Coord box): Bool =>
    (x != that.x) or (y != that.y)

  fun add(other: Coord): Coord =>
    Coord(x + other.x, y + other.y)

  fun copy(): Coord =>
    Coord(x, y)

  fun string(): String =>
    "(" + x.string() + ", " + y.string() + ")"


class Blizzard
  var pos: Coord
  var dir: Coord

  new create(x: I32, y: I32, dir': U8) =>
    pos = Coord(x, y)
    dir = match dir'
      | '>' => Coord( 1,  0)
      | '<' => Coord(-1,  0)
      | '^' => Coord( 0, -1)
      | 'v' => Coord( 0,  1)
      else
        Coord(0, 0)
      end


class State
  // The current time. Equal to number of steps (including in-place) taken.
  let minute: I32

  // Position of the player.
  let pos: Coord

  new create(minute': I32, pos': Coord) =>
    minute = minute'
    pos = pos'

actor Main
  let env: Env

  let blizzards: Array[Blizzard]

  // For every minute, all of the coordinates that are occupied by a blizzard.
  let block_maps: Array[Set[Coord]]

  // Dimensions of the board, excluding the walls.
  let width: I32
  let height: I32

  new create(env': Env) =>
    env = env'
    blizzards = []
    block_maps = []

    var h: I32 = 0
    var w: I32 = 0

    try
      let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
      let path = FilePath(FileAuth(env.root), "example.txt", caps)
      let open_result = OpenFile(path)
      let file = open_result as File

      for (y, file_line) in Iter[String](file.lines()).enum() do
        // We exclude the walls from the size of the board.
        w = file_line.size().i32() - 2
        h = y.i32() - 1
        for (x, cell) in Iter[U8](file_line.values()).enum() do
          if (cell == '.') or (cell == '#') then
            continue
          else
            blizzards.push(Blizzard(x.i32() - 1, y.i32() - 1, cell))
          end
        end
      end
    else
      env.out.print("Something went wrong while reading input.")
    end

    width = w
    height = h
