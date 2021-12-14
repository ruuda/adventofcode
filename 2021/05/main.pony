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

// An axis-aligned line. The axis is not specified out of band.
class AaLine
  let x: U32
  let y: U32
  let len: U32

  new create(x': U32, y': U32, len': U32) =>
    x = x'
    y = y'
    len = len'


actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      var hlines: Array[AaLine] = []
      var vlines: Array[AaLine] = []

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

        if line.x0 == line.x1 then
          if line.y1 > line.y0 then line.y1
            vlines.push(AaLine(line.x0, line.y0, line.y1 - line.y0))
          else
            vlines.push(AaLine(line.x0, line.y1, line.y0 - line.y1))
          end
        elseif line.y0 == line.y1 then
          if line.x1 > line.x0 then
            hlines.push(AaLine(line.x0, line.y0, line.x1 - line.x0))
          else
            hlines.push(AaLine(line.x1, line.y0, line.x0 - line.x1))
          end
        else
          // For now (part 1), we ignore non-axis-aligned lines.
          continue
        end

      end

      for z in hlines.values() do
        env.out.print(z.x.string() + "," + z.y.string() + " -> " + z.len.string())
      end

    else
      env.out.print("Something went wrong")
    end
