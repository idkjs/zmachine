open Utility;
open Type;

type t = {
  stack: Evaluation_stack.t,
  local_store: Local_store.t,
  resume_at: instruction_address,
  store: option(variable_location),
};

let empty = {
  stack: Evaluation_stack.empty,
  local_store: Local_store.empty,
  resume_at: Instruction(0),
  store: None,
};

let resume_at = frame => frame.resume_at;

let store = frame => frame.store;

let peek_stack = frame => Evaluation_stack.peek(frame.stack);

let pop_stack = frame => {
  ...frame,
  stack: Evaluation_stack.pop(frame.stack),
};

let push_stack = (frame, value) => {
  ...frame,
  stack: Evaluation_stack.push(frame.stack, value),
};

let write_local = (frame, local, value) => {
  ...frame,
  local_store: Local_store.write_local(frame.local_store, local, value),
};

let read_local = (frame, local) =>
  Local_store.read_local(frame.local_store, local);

let display = frame => {
  let Instruction(resume_at) = frame.resume_at;
  let locals = Local_store.display(frame.local_store);
  let stack = Evaluation_stack.display(frame.stack);
  Printf.sprintf(
    "Locals %s\nStack %s\nResume at:%04x\n",
    locals,
    stack,
    resume_at,
  );
};
