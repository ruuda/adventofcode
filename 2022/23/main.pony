use "files"
use "collections"
use "itertools"

class Coord
  let x: I32
  let y: I32

  new create(x': I32, y': I32) =>
    x = x'
    y = y'

  fun add(other: Coord): Coord =>
    Coord(x + other.x, y + other.y)

  fun string(): String =>
    "(" + x.string() + ", " + y.string() + ")"


class Elf
  var pos: Coord
  var next_dir: Array[Coord]
  var next_pos: Coord

  new create(x: I32, y: I32) =>
    pos = Coord(x, y)
    // Elves start out wanting to go north, south, west, east.
    next_dir = [Coord(0, 1); Coord(0, -1); Coord(-1, 0); Coord(1, 0)]
    next_pos = Coord(x, y)

  fun ref propose_next(board: SetIs[Coord]) =>
      for dir in next_dir.values() do
        let forward = pos.add(dir)
        let left = Coord(-dir.y, dir.x).add(forward)
        let right = Coord(dir.y, -dir.x).add(forward)
        let target_is_occupied = (
          false
          or board.contains(forward)
          or board.contains(left)
          or board.contains(right)
        )
        if not target_is_occupied then
          next_pos = forward
          break
        end
      end


actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      var elves: Array[Elf] = []

      let path = FilePath(FileAuth(env.root), "example2.txt", caps)
      let open_result = OpenFile(path)
      let file = open_result as File

      for (y, file_line) in Iter[String](file.lines()).enum() do
        for (x, cell) in Iter[U8](file_line.values()).enum() do
          if cell == '#' then
            elves.push(Elf(x.i32(), -y.i32()))
          end
        end
      end

      for elf in elves.values() do
        env.out.print("Elf at " + elf.pos.string())
      end

    else
      env.out.print("Something went wrong")
    end
