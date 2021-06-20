open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let instruction = Instruction.decode(story, Instruction(0x37d9));
  let text = Instruction.display(instruction, Story.version(story));
  Printf.printf("%s\n", text);
};
