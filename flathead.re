open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let dict = Dictionary.display(story);
  Printf.printf("%s\n", dict);
};
