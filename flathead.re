open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let locals = Local_store.create_default_locals(story, Routine(0x3b36));
  let text = Local_store.display(locals);
  Printf.printf("%s\n", text);
};
