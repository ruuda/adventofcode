use "files"
use "collections"

actor Main
  fun bitwise_most_common(width: USize, numbers: Array[U32]): U32 =>
    try
      let counts: Array[U32] = []
      for i in Range(0, width) do
        counts.push(0)
      end

      for n in numbers.values() do
        for i in Range(0, width) do
          let bit = (n >> (width - 1 - i).u32()) and 1
          counts(i)? = counts(i)? + bit
        end
      end

      var result: U32 = 0
      for count in counts.values() do
        let bit: U32 = if count >= (numbers.size().u32() - count) then 1 else 0 end
        result = (result << 1) + bit
      end
      result

    else
      // Should be impossible, I would like my program to crash/panic on out of
      // bounds access, but apparently that is not how Pony works?
      0
    end

  fun bitwise_least_common(width: USize, numbers: Array[U32]): U32 =>
    (0xffffffff >> (32 - width.u32())) and (not bitwise_most_common(width, numbers))

  new create(env: Env) =>
    let caps = recover val FileCaps.>set(FileRead).>set(FileStat) end
    try
      // We will also convert the numbers from ASCII binary digits to U32.
      var numbers: Array[U32] = []
      var width: USize = 0

      let base = env.root as AmbientAuth
      let path = FilePath(base, "input.txt", caps)?
      let open_result = OpenFile(path)
      let file = open_result as File
      for line in file.lines() do
        var n: U32 = 0
        var n_width: USize = 0
        for char in (consume line).values() do
          let bit = char - '0'
          n = (n << 1) + bit.u32()
          n_width = n_width + 1
        end
        numbers.push(n)
        width = n_width
      end

      let gamma_rate = bitwise_most_common(width, numbers)
      let epsilon_rate = bitwise_least_common(width, numbers)
      env.out.print("Gamma rate:   " + gamma_rate.string())
      env.out.print("Epsilon rate: " + epsilon_rate.string())

      let result1 = gamma_rate * epsilon_rate
      env.out.print("Part 1: " + result1.string())

      var oxygen_candidates = numbers.clone()
      var i = width.u32()
      while oxygen_candidates.size() > 1 do
        let mask = bitwise_most_common(width, oxygen_candidates)
        i = i - 1
        let new_candidates: Array[U32] = []
        for n in oxygen_candidates.values() do
          if ((n >> i) and 1) == ((mask >> i) and 1) then
            new_candidates.push(n)
          end
        end
        oxygen_candidates = new_candidates
      end
      let oxygen_rating = oxygen_candidates(0)?

      var co2_candidates = numbers.clone()
      i = width.u32()
      while co2_candidates.size() > 1 do
        let mask = bitwise_least_common(width, co2_candidates)
        i = i - 1
        let new_candidates: Array[U32] = []
        for n in co2_candidates.values() do
          if ((n >> i) and 1) == ((mask >> i) and 1) then
            new_candidates.push(n)
          end
        end
        co2_candidates = new_candidates
      end
      let co2_rating = co2_candidates(0)?

      env.out.print("Oxygen generator rating: " + co2_rating.string())
      env.out.print("CO2 scrubber rating:     " + oxygen_rating.string())
      let result2 = co2_rating * oxygen_rating
      env.out.print("Part 2: " + result2.string())

    else
      env.out.print("Something went wrong")
    end
