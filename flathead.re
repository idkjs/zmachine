open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let table = Object.display_object_table(story);
  Printf.printf("%s\n", table);
};
