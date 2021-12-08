use "files"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      let base = env.root as AmbientAuth
      let path = FilePath(base, "./test.txt", caps)?
      let open_result = OpenFile(path)
      with file = open_result as File do
        for line in file.lines() do
          for char in (consume line).values() do
            let bit = char - '0'
            env.out.print(bit.string())
          end
        end
      end
    else
      env.out.print("Something went wrong")
    end
