/* The frame set is the stack of activation frames; each activation frame
   has a local variable storage, evaluation stack, and information about
   the call site that created this activation. */

open Type;
open Utility;

type t = {
  initial_frame: Frame.t,
  frames: list(Frame.t),
};

let make = initial_frame => {initial_frame, frames: []};

let current_frame = frameset =>
  switch (frameset.frames) {
  | [] => frameset.initial_frame
  | [h, ..._] => h
  };

let set_current_frame = (frameset, frame) =>
  switch (frameset.frames) {
  | [] => {...frameset, initial_frame: frame}
  | [_, ...t] => {...frameset, frames: [frame, ...t]}
  };

let add_frame = (frameset, frame) => {
  ...frameset,
  frames: [frame, ...frameset.frames],
};

let remove_frame = frameset =>
  switch (frameset.frames) {
  | [] => failwith("Attempting to remove initial frame")
  | [_, ...t] => {...frameset, frames: t}
  };

let peek_stack = frameset => Frame.peek_stack(current_frame(frameset));

let pop_stack = frameset =>
  set_current_frame(frameset, Frame.pop_stack(current_frame(frameset)));

let push_stack = (frameset, value) =>
  set_current_frame(
    frameset,
    Frame.push_stack(current_frame(frameset), value),
  );

let read_local = (frameset, local) =>
  Frame.read_local(current_frame(frameset), local);

let write_local = (frameset, local, value) =>
  set_current_frame(
    frameset,
    Frame.write_local(current_frame(frameset), local, value),
  );

let display = frameset =>
  accumulate_strings(Frame.display, frameset.frames)
  ++ Frame.display(frameset.initial_frame);
