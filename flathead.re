open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let tree = Object.display_object_tree(story);
  Printf.printf("%s\n", tree);
};
