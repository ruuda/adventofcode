use "files"

actor Main
  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      // For every bit, we keep track of how often it occurs.
      var bits: Array[U32] = []
      var num_lines: U32 = 0

      let base = env.root as AmbientAuth
      let path = FilePath(base, "input.txt", caps)?
      let open_result = OpenFile(path)
      let file = open_result as File
      for line in file.lines() do
        num_lines = num_lines + 1
        var i: USize = 0
        for char in (consume line).values() do
          let bit = char - '0'
          if bits.size() <= i then
            bits.push(bit.u32())
          else
            bits(i)? = bits(i)? + bit.u32()
          end
          i = i + 1
        end
      end

      // Now output the bitstring, printing the most common bit at each index.
      var gamma_rate: U32 = 0
      var epsilon_rate: U32 = 0
      for count in bits.values() do
        let bit: U32 = if count > (num_lines / 2) then 1 else 0 end
        gamma_rate = (gamma_rate << 1) + bit
        epsilon_rate = (epsilon_rate << 1) + (1 - bit)
      end
      let result = gamma_rate * epsilon_rate
      env.out.print(result.string())

    else
      env.out.print("Something went wrong")
    end
