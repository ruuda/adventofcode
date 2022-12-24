use "collections"
use "files"
use "itertools"
use "time"

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

  fun min_minute(): I32 =>
    """
    Return the minimal time at which we can reach the exit from this state.
    Lower is better.
    """
    minute + min_distance

  fun compare(that: State box): (Less | Equal | Greater) =>
    // We order states by the minimum time to reach the exit from there.
    if min_minute() < that.min_minute() then return Less end
    if min_minute() > that.min_minute() then return Greater end

    // Then break ties by preferring states later in time.
    if minute > that.minute then return Less end
    if minute < that.minute then return Greater end

    // Then break ties by preferring states closer to the exit.
    if min_distance < that.min_distance then return Less end
    if min_distance > that.min_distance then return Greater end

    if pos.y < that.pos.y then return Less end
    if pos.y > that.pos.y then return Greater end

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


primitive StartToEnd
primitive EndToStart

actor Simulator
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

  var phase: I32

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
      step_blizzards()
    end
    try
      closed_at(minute.usize())?
    else
      // Should not happen, but I have no way to panic ...
      Set[Coord]()
    end

  be inspect_one(i: I32) =>
    """
    Inspect the next state. If we can reach the exit from there, return the
    number of minutes it took. If we can't, return error.
    """
    let state = try
      open.pop()?
    else
      env.out.print("Error: work queue underrun.")
      return
    end

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

        if next_pos == exit_pos then
          env.out.print("Minimal minute " + m.string())
          // TODO: Send result to main actor.

          // The previous set of closed nodes, and the remaining open nodes, are
          // no longer interesting. We can free up the memory after this
          // behavior exits.  It is important that we free them before
          // returning, and not at the start of the behavior, because GC only
          // runs int between behaviors.
          open.clear()
          for old_closed in closed_at.values() do
            old_closed.clear()
          end

          // If we have more to compute, schedule the continuation.
          phase = phase + 1
          match phase
            | 1 => navigate(EndToStart)
            | 2 => navigate(StartToEnd)
          end

          return
        end

        let is_out_of_bounds = (
          false
          or (next_pos.x < 0)
          or (next_pos.x >= width)
          or (next_pos.y < 0)
          or (next_pos.y >= height)
        )
        if is_out_of_bounds and (next_pos != state.pos) then
          continue
        end

        if not closed.contains(next_pos) then
          open.push(State(m, exit_pos.manhattan(next_pos), next_pos))
        end
      end
    end

    // If we get here, then we did not find the exit. Print a status update once
    // in a while.
    if (i % 10000) == 0 then
      try
        let best = open.peek()?
        env.out.print(
          " i=" + i.string() +
          " max_explored_minute=" + (closed_at.size() - 1).string() +
          " open=" + open.size().string() +
          " minute=" + best.minute.string() +
          " min_distance=" + best.min_distance.string() +
          " min_solve_minute=" + (best.minute + best.min_distance).string()
        )
      end
    end

    inspect_one(i + 1)

  be navigate(dir: (StartToEnd | EndToStart)) =>
    """
    Compute the minimum minute where we reach the target.
    Time continues to advance when called multiple times.
    """
    (let start_pos, let end_pos) = match dir
      | StartToEnd => (Coord(0, -1), Coord(width - 1, height))
      | EndToStart => (Coord(width - 1, height), Coord(0, -1))
    end

    let start_minute = closed_at.size().i32() - 1
    exit_pos = end_pos.clone()

    // We start out one position outside of the board, in the top-left.
    let initial_state = State(start_minute, exit_pos.manhattan(start_pos), start_pos.clone())
    open.push(initial_state)

    env.out.print(
      "Starting search from " + start_pos.string() +
      " to " + end_pos.string() +
      " at minute " + start_minute.string() +
      " with " + open.size().string() + " open nodes."
    )

    inspect_one(0)

  new create(env': Env) =>
    env = env'
    blizzards = []
    closed_at = []
    open = MinHeap[State](0)

    var h: I32 = 0
    var w: I32 = 0

    try
      let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
      let path = FilePath(FileAuth(env.root), "input.txt", caps)
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
    exit_pos = Coord(0, 0)
    phase = 0

    // Mark the blizzard positions at minute 0.
    closed_at.push(mark_blizzards())


actor Main
  new create(env: Env) =>
    let simulator: Simulator = Simulator(env)

    // Pony does not run GC at arbitrary times. Therefore, we need to break up
    // our long-running computation into behaviors, to allow the GC to run in
    // between. Kick off the initial phase now. It will schedule its own
    // continueation.
    simulator.navigate(StartToEnd)
