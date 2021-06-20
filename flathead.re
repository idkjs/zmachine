open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let zstring = Zstring.abbreviation_zstring(story, Abbreviation(0));
  let text = Zstring.display_bytes(story, zstring);
  Printf.printf("%s\n", text);
  let zstring = Zstring.abbreviation_zstring(story, Abbreviation(4));
  let text = Zstring.display_bytes(story, zstring);
  Printf.printf("%s\n", text);
};
