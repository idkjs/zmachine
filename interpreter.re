open Utility;
open Type;

type t = {
  story: Story.t,
  program_counter: instruction_address,
  frames: Frameset.t,
};

let make = story => {
  story,
  program_counter: Story.initial_program_counter(story),
  frames: Frameset.make(Frame.empty),
};

let current_frame = interpreter => Frameset.current_frame(interpreter.frames);

let add_frame = (interpreter, frame) => {
  ...interpreter,
  frames: Frameset.add_frame(interpreter.frames, frame),
};

let remove_frame = interpreter => {
  ...interpreter,
  frames: Frameset.remove_frame(interpreter.frames),
};

let peek_stack = interpreter => Frameset.peek_stack(interpreter.frames);

let pop_stack = interpreter => {
  ...interpreter,
  frames: Frameset.pop_stack(interpreter.frames),
};

let push_stack = (interpreter, value) => {
  ...interpreter,
  frames: Frameset.push_stack(interpreter.frames, value),
};

let program_counter = interpreter => interpreter.program_counter;

let set_program_counter = (interpreter, program_counter) => {
  ...interpreter,
  program_counter,
};

let read_local = (interpreter, local) =>
  Frameset.read_local(interpreter.frames, local);

let write_local = (interpreter, local, value) => {
  ...interpreter,
  frames: Frameset.write_local(interpreter.frames, local, value),
};

let read_global = (interpreter, global) =>
  Globals.read(interpreter.story, global);

let write_global = (interpreter, global, value) => {
  ...interpreter,
  story: Globals.write(interpreter.story, global, value),
};

let read_variable = (interpreter, variable) =>
  switch (variable) {
  | Stack => (peek_stack(interpreter), pop_stack(interpreter))
  | Local_variable(local) => (read_local(interpreter, local), interpreter)
  | Global_variable(global) => (
      read_global(interpreter, global),
      interpreter,
    )
  };

let write_variable = (interpreter, variable, value) =>
  switch (variable) {
  | Stack => push_stack(interpreter, value)
  | Local_variable(local) => write_local(interpreter, local, value)
  | Global_variable(global) => write_global(interpreter, global, value)
  };

let read_operand = (interpreter, operand) =>
  switch (operand) {
  | Large(large) => (large, interpreter)
  | Small(small) => (small, interpreter)
  | Variable(v) => read_variable(interpreter, v)
  };

/* Takes a list of operands, produces a list of arguments. */
let operands_to_arguments = (interpreter, operands) => {
  let rec aux = ((args, interp), ops) =>
    switch (ops) {
    | [] => (args, interp)
    | [h, ...t] =>
      let (argument, new_interpreter) = read_operand(interp, h);
      aux(([argument, ...args], new_interpreter), t);
    };
  let (args_rev, final_interpreter) = aux(([], interpreter), operands);
  (List.rev(args_rev), final_interpreter);
};

let interpret_store = (interpreter, store, result) =>
  switch (store) {
  | None => interpreter
  | Some(variable) => write_variable(interpreter, variable, result)
  };

let display_current_instruction = interpreter => {
  let address = interpreter.program_counter;
  let instruction = Instruction.decode(interpreter.story, address);
  Instruction.display(instruction, interpreter.story);
};

/* Debugging method */
let display = interpreter => {
  let frames = Frameset.display(interpreter.frames);
  let instr = display_current_instruction(interpreter);
  Printf.sprintf("\n---\n%s\n%s\n", frames, instr);
};

/* This routine handles all call instructions:

   2OP:25  call_2s  routine arg -> (result)
   2OP:26  call_2n  routine arg
   1OP:136 call_1s  routine -> (result)
   1OP:143 call_1n  routine
   VAR:224 call_vs  routine up-to-3-arguments -> (result)
   VAR:236 call_vs2 routine up-to-7-arguments -> (result)
   VAR:249 call_vn  routine up-to-3-arguments
   VAR:250 call_vn2 routine up-to-7-arguments

   The "s" versions store the result; the "n" versions discard it. */

let handle_call = (routine_address, arguments, interpreter, instruction) =>
  if (routine_address == 0) {
    /* Spec: When the address 0 is called as a routine, nothing happens and the
       return value is false. */
    let result = 0;
    let store = Instruction.store(instruction);
    let store_interpreter = interpret_store(interpreter, store, result);
    let addr = Instruction.following(instruction);
    set_program_counter(store_interpreter, addr);
  } else {
    let routine_address = Packed_routine(routine_address);
    let routine_address =
      Story.decode_routine_packed_address(interpreter.story, routine_address);
    let resume_at = Instruction.following(instruction);
    let store = Instruction.store(instruction);
    let frame =
      Frame.make(
        interpreter.story,
        arguments,
        routine_address,
        resume_at,
        store,
      );
    let pc = Routine.first_instruction(interpreter.story, routine_address);
    set_program_counter(add_frame(interpreter, frame), pc);
  };

/* Move the interpreter on to the next instruction */
let step_instruction = interpreter => {
  let instruction =
    Instruction.decode(interpreter.story, interpreter.program_counter);
  let operands = Instruction.operands(instruction);
  let (arguments, interpreter) =
    operands_to_arguments(interpreter, operands);
  let opcode = Instruction.opcode(instruction);
  switch (opcode, arguments) {
  | (VAR_224, [routine, ...args]) =>
    handle_call(routine, args, interpreter, instruction)
  | _ =>
    failwith(
      Printf.sprintf(
        "TODO: %s ",
        Instruction.display(instruction, interpreter.story),
      ),
    )
  };
};
/* End step_instruction */
