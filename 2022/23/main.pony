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


class Elf
  var pos: Coord
  var next_dir: Array[Coord]
  var next_pos: Coord

  new create(x: I32, y: I32) =>
    pos = Coord(x, y)
    // Elves start out wanting to go north, south, west, east.
    next_dir = [Coord(0, 1); Coord(0, -1); Coord(-1, 0); Coord(1, 0)]
    next_pos = Coord(x, y)

  fun ref propose_next(board: Set[Coord]) =>
      // If we cannot find a new place to move to, we will stay here.
      next_pos = pos

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

      // Next time we try to find a position, the elf will try in a different
      // order. Note, the shift will not fail, we always have four elements.
      try
        let dir = next_dir.shift()?
        next_dir.push(dir)
      end

  fun ref move_next(proposals: Map[Coord, U32]) =>
    // See how many elves wanted to move to the same position as we did. If it's
    // only one, then that was us, and we can move there. Note, the position
    // must exist in `proposals`, but as far as I am aware Pony has no concept
    // of panic, so we have to pick a value.
    let demand = try proposals(next_pos)? else 0 end
    if demand == 1 then
      pos = next_pos
    else
      next_pos = pos
    end

actor Main
  var elves: Array[Elf]
  var env: Env

  fun get_board(): Set[Coord] =>
    """
    Return all board positions where an elf is currently located.
    """
    let board = Set[Coord]()
    for elf in elves.values() do
      board.set(elf.pos.copy())
    end
    board

  fun get_next_board(): Map[Coord, U32] =>
    """
    Return per board position, how many elves plan to move there.
    """
    let board = Map[Coord, U32]()
    for elf in elves.values() do
      board.upsert(elf.next_pos.copy(), 1, {(old, x) => old + x})
    end
    board

  fun ref step_phase1() =>
    """
    Let the elves decide where they want to move next.
    """
    let board = get_board()
    for elf in elves.values() do
      elf.propose_next(board)
    end

  fun ref step_phase2() =>
    """
    Move the elves to where they want to go if possible.
    """
    let next_board = get_next_board()
    for elf in elves.values() do
      elf.move_next(next_board)
    end

  fun bounds(): (Coord, Coord) =>
    """
    Return the minimum (x, y) and maximum (x, y) of elf positions.
    """
    try
      (var min_x, var min_y) = (elves(0)?.pos.x, elves(0)?.pos.y)
      (var max_x, var max_y) = (elves(0)?.pos.x, elves(0)?.pos.y)
      for elf in elves.values() do
        (min_x, min_y) = (min_x.min(elf.pos.x), min_y.min(elf.pos.y))
        (max_x, max_y) = (max_x.max(elf.pos.x), max_y.max(elf.pos.y))
      end
      (Coord(min_x, min_y), Coord(max_x, max_y))
    else
      (Coord(0, 0), Coord(0, 0))
    end

  fun print_board() =>
    (let min, let max) = bounds()
    let board = get_board()
    for y in Reverse[I32](max.y, min.y) do
      for x in Range[I32](min.x, max.x + 1) do
        if board.contains(Coord(x, y)) then
          env.out.write("#")
        else
          env.out.write(".")
        end
      end
      env.out.print("")
    end

  new create(env': Env) =>
    env = env'
    elves = []
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end

    try
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
    else
      env.out.print("Something went wrong while reading input.")
    end

    for elf in elves.values() do
      env.out.print("Elf at " + elf.pos.string())
    end
    print_board()
    step_phase1()
    step_phase2()
    print_board()
    step_phase1()
    step_phase2()
    print_board()
