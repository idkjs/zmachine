open Type;
open Utility;

let locals_count = (story, Routine(routine_address)) =>
  Story.read_byte(story, Byte_address(routine_address));

let first_instruction = (story, Routine(routine_address)) =>
  if (Story.v4_or_lower(Story.version(story))) {
    let count = locals_count(story, Routine(routine_address));
    Instruction(routine_address + 1 + count * word_size);
  } else {
    Instruction(routine_address + 1);
  };

let local_default_value = (story, Routine(routine_address), n) =>
  if (Story.v4_or_lower(Story.version(story))) {
    let addr = Word_address(routine_address + 1);
    Story.read_word(story, inc_word_addr_by(addr, n - 1));
  } else {
    0;
  };
