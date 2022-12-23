use "files"
use "collections"
use "itertools"

class Elf
  var x: I32
  var y: I32

  new create(x': I32, y': I32) =>
    x = x'
    y = y'


actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      var elves: Array[Elf] = []

      let path = FilePath(FileAuth(env.root), "example.txt", caps)
      let open_result = OpenFile(path)
      let file = open_result as File

      for (y, file_line) in Iter[String](file.lines()).enum() do
        for (x, cell) in Iter[U8](file_line.values()).enum() do
          if cell == '#' then
            elves.push(Elf(x.i32(), y.i32()))
          end
        end
      end

      for elf in elves.values() do
        env.out.print("Elf at " + elf.x.string() + ", " + elf.y.string())
      end

    else
      env.out.print("Something went wrong")
    end
