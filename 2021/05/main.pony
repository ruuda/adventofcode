use "files"
use "collections"

class Line
  let x0: U32
  let y0: U32
  let x1: U32
  let y1: U32

  new create(x0': U32, y0': U32, x1': U32, y1': U32) =>
    x0 = x0'
    y0 = y0'
    x1 = x1'
    y1 = y1'


actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      var lines: Array[Line] = []

      let base = env.root as AmbientAuth
      let path = FilePath(base, "test.txt", caps)?
      let open_result = OpenFile(path)
      let file = open_result as File
      for file_line in file.lines() do
        let coords = file_line.split_by(" -> ")
        let coord0 = coords(0)?.split_by(",")
        let coord1 = coords(1)?.split_by(",")
        let line = Line(
          coord0(0)?.u32()?,
          coord0(1)?.u32()?,
          coord1(0)?.u32()?,
          coord1(1)?.u32()?
        )
        lines.push(line)
      end

      for z in lines.values() do
        env.out.print(z.x0.string() + "," + z.y0.string() + " -> " + z.x1.string())
      end


    else
      env.out.print("Something went wrong")
    end
