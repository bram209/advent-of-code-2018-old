let to_cycling_stream = items => {
  let buf = ref([]);
  let rec next = i => {
    if (buf^ == []) {
      buf := items;
    };
    switch (buf^) {
    | [h, ...t] =>
      buf := t;
      Some(h);
    | [] => None
    };
  };
  Stream.from(next);
};

let rec stream_fold_until = (f, acc, stream) => {
  let next = Stream.peek(stream);
  let _ = Stream.junk(stream);
  switch (next) {
  | None => acc
  | Some(next) =>
    let (halted, next_acc) = f(next, acc);
    if (halted) {
      acc;
    } else {
      stream_fold_until(f, next_acc, stream);
    };
  };
};

let stream_fold = (f, acc, stream) => {
  let result = ref(acc);
  Stream.iter(x => result := f(x, result^), stream);
  result^;
};

let read_lines = filename => {
  let lines = ref([]);
  let chan = open_in(filename);
  try (
    {
      while (true) {
        lines := [input_line(chan), ...lines^];
      };
      [];
    }
  ) {
  | End_of_file =>
    close_in(chan);
    List.rev(lines^);
  };
};