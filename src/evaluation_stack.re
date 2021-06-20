open Utility;

type t = {items: list(int)};

let empty = {items: []};

let length = stack => List.length(stack.items);

let peek = stack =>
  switch (stack.items) {
  | [] => failwith("peeking an empty stack")
  | [h, ..._] => h
  };

let pop = stack =>
  switch (stack.items) {
  | [] => failwith("popping empty stack")
  | [_, ...t] => {items: t}
  };

let push = (stack, item) => {
  let item = unsigned_word(item);
  {items: [item, ...stack.items]};
};

let display = stack => {
  let to_string = item => Printf.sprintf(" %04x", item);
  accumulate_strings(to_string, stack.items);
};
