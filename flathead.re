open Type;
open Utility;

let () = {
  let story = Story.load("minizork.z3");
  let interpreter1 = Interpreter.make(story);
  let interpreter2 = Interpreter.step_instruction(interpreter1);
  let interpreter3 = Interpreter.step_instruction(interpreter2);
  let text1 = Interpreter.display(interpreter1);
  let text2 = Interpreter.display(interpreter2);
  let text3 = Interpreter.display(interpreter3);
  Printf.printf("%s\n%s\n%s\n", text1, text2, text3);
};
