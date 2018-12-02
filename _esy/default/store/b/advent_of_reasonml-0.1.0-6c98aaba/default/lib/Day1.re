module IntSet =
  Set.Make({
    let compare = Pervasives.compare;
    type t = int;
  });

let solve = () =>
  Util.read_lines("day1.txt")
  |> List.map(line => int_of_string(line))
  |> List.fold_left((freq, acc) => acc + freq, 0);

let solve2 = () => {
  let (end_freq, _) =
    Util.read_lines("day1.txt")
    |> List.map(line => int_of_string(line))
    |> Util.to_cycling_stream
    |> Util.stream_fold_until(
         (freq, (computed_freq, history)) => {
           let new_freq = computed_freq + freq;
           if (IntSet.mem(new_freq, history)) {
             (true, (computed_freq, history));
           } else {
             let new_history = IntSet.add(new_freq, history);
             (false, (new_freq, new_history));
           };
         },
         (0, IntSet.empty),
       );

  end_freq;
};