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

  fun clone(): Coord =>
    Coord(x, y)

  fun string(): String =>
    "(" + x.string() + ", " + y.string() + ")"

  fun manhattan(that: Coord box): I32 =>
    """
    Return the distance between this and that using the Manhattan metric.
    """
    ((x - that.x).abs() + (y - that.y).abs()).i32()


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

  fun ref move(w: I32, h: I32) =>
    pos = pos.add(dir)
    if pos.x <  0 then pos = pos.add(Coord( w, 0)) end
    if pos.x >= w then pos = pos.add(Coord(-w, 0)) end
    if pos.y <  0 then pos = pos.add(Coord(0,  h)) end
    if pos.y >= h then pos = pos.add(Coord(0, -h)) end

class State
  // The current time. Equal to number of steps (including in-place) taken.
  let minute: I32

  // The minimum number of minutes that it would take to reach the exit.
  let min_distance: I32

  // Position of the player.
  let pos: Coord

  new create(minute': I32, min_distance': I32, pos': Coord) =>
    minute = minute'
    min_distance = min_distance'
    pos = pos'

  fun compare(that: State box): (Less | Equal | Greater) =>
    // We order states by the minimum time to reach the exit from there.
    let n = minute + min_distance
    let m = that.minute + that.min_distance

    if n < m then return Less end
    if n > m then return Greater end

    // Then break ties by preferring states closer to the exit.
    if min_distance < that.min_distance then return Less end
    if min_distance > that.min_distance then return Greater end

    // Then break ties by preferring states later in time.
    if minute > that.minute then return Less end
    if minute < that.minute then return Greater end

    if pos.y > that.pos.y then return Less end
    if pos.y < that.pos.y then return Greater end

    if pos.x > that.pos.x then return Less end
    if pos.x < that.pos.x then return Greater end

    Equal

  fun eq(that: State box): Bool =>
    (minute == that.minute) and (pos == that.pos)

  fun ne(that: State box): Bool =>
    (minute != that.minute) or (pos != that.pos)

  fun lt(that: State box): Bool =>
    compare(that) == Less

  fun le(that: State box): Bool =>
    (compare(that) == Less) or (compare(that) == Equal)

  fun gt(that: State box): Bool =>
    compare(that) == Greater

  fun ge(that: State box): Bool =>
    (compare(that) == Greater) or (compare(that) == Equal)


actor Main
  let env: Env

  let blizzards: Array[Blizzard]

  // For every minute, all locations that either:
  // * Are occupied by a blizzard.
  // * Have already been inspected.
  let closed_at: Array[Set[Coord]]

  // States to investigate.
  let open: MinHeap[State]

  // Dimensions of the board, excluding the walls.
  let width: I32
  let height: I32
  var exit_pos: Coord

  fun mark_blizzards(): Set[Coord] =>
    var closed = Set[Coord]()
    for blizzard in blizzards.values() do
      closed.set(blizzard.pos.clone())
    end
    closed

  fun ref step_blizzards() =>
    for blizzard in blizzards.values() do
      blizzard.move(width, height)
    end
    closed_at.push(mark_blizzards())

  fun ref ensure_closed_at(minute: I32): Set[Coord] ref =>
    while closed_at.size().i32() <= minute do
      env.out.print("Advanced blizzards to minute " + minute.string() + ".")
      step_blizzards()
    end
    try
      closed_at(minute.usize())?
    else
      // Should not happen, but I have no way to panic ...
      Set[Coord]()
    end

  fun ref inspect_one(): I32? =>
    """
    Inspect the next state. If we can reach the exit from there, return the
    number of minutes it took. If we can't, return error.
    """
    let state = try open.pop()? else return -1 end

    env.out.print(" state pos=" + state.pos.string() + " min_dist=" + state.min_distance.string())

    // We now know a way to reach `state.pos` in minute `state.minute`, no need
    // to revisit that.
    ensure_closed_at(state.minute).set(state.pos)

    let m = state.minute + 1
    let closed: Set[Coord] ref = ensure_closed_at(m)

    for dy in Range[I32](-1, 2) do
      for dx in Range[I32](-1, 2) do
        // We can only step horizontally or vertically (or not at all),
        // not diagonally.
        if (dx.abs() + dy.abs()) > 1 then continue end

        let next_pos = state.pos.add(Coord(dx, dy))
        env.out.write("   consider " + next_pos.string())

        if next_pos == exit_pos then
          env.out.print(" is exit")
          return m
        end

        let is_out_of_bounds = (
          false
          or (next_pos.x < 0)
          or (next_pos.x >= width)
          or (next_pos.y < 0)
          or (next_pos.y >= height)
        )
        if is_out_of_bounds and (next_pos != state.pos) then
          env.out.print(" is oob")
          continue
        end

        if not closed.contains(next_pos) then
          env.out.print(" -> pushed")
          open.push(State(m, exit_pos.manhattan(next_pos), next_pos))
        else
          env.out.print(" is closed")
        end
      end
    end

    // If we did not return, then we did not find the exit.
    error

  fun ref navigate_from_to(start_pos: Coord, end_pos: Coord) =>
    """
    Return the minimum number of minutes to go from the start pos to the end
    pos. Time continues to advance when called multiple times.
    """
    let start_minute = closed_at.size().i32() - 1
    exit_pos = end_pos

    // We start out one position outside of the board, in the top-left.
    let initial_state = State(start_minute, exit_pos.manhattan(start_pos), start_pos)
    open.clear()
    open.push(initial_state)

    // If the closed nodes are already there, there might be pollution from a
    // previous exploration, recompute them for the next timestep to have only
    // the blizzards as closed nodes, not visited places.
    // try
    //   closed_at.pop()?
    //   closed_at.push(mark_blizzards())
    //   env.out.print("Refreshed closed_at for minute " + (closed_at.size() - 1).string())
    // end

    env.out.print(
      "Starting search from " + start_pos.string() +
      " to " + end_pos.string() +
      " at minute " + start_minute.string() +
      " with " + open.size().string() + " open nodes."
    )

    var i: I32 = 0
    while true do
      try
        let n = inspect_one()?
        env.out.print(
          "Minimal minute to go from " + start_pos.string() +
          " to " + end_pos.string() + ": " + n.string()
        )
        break
      else
        if (i % 10000) == 0 then
          env.out.print(
            " i=" + i.string() +
            " max_minute=" + (closed_at.size() - 1).string() +
            " open=" + open.size().string()
          )
        end
        i = i + 1
      end
    end

  new create(env': Env) =>
    env = env'
    blizzards = []
    closed_at = []
    open = MinHeap[State](0)

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
    let start_pos = Coord(0, -1)
    let end_pos = Coord(width - 1, height)
    exit_pos = end_pos

    // Mark the blizzard positions at minute 0.
    closed_at.push(mark_blizzards())

    navigate_from_to(start_pos, end_pos)
    navigate_from_to(end_pos, start_pos)
    navigate_from_to(start_pos, end_pos)
