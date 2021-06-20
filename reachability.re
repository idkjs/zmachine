open Utility;
open Type;

/* Any given instruction in a routine either goes on to the next instruction,
   when it is done, or branches to another instruction when it is done, or terminates
   the routine. Given the address of an instruction, what are all the reachable instructions
   in this routine? Note that this could miss instructions if a jump is made to a location
   read from a variable. */

let following_instruction = instr =>
  if (Instruction.continues_to_following(Instruction.opcode(instr))) {
    let Instruction(addr) = Instruction.address(instr);
    let length = Instruction.length(instr);
    [Instruction(addr + length)];
  } else {
    [];
  };

let branch_target_instruction = instr =>
  switch (Instruction.branch(instr)) {
  | None
  | Some((_, Return_false))
  | Some((_, Return_true)) => []
  | Some((_, Branch_address(address))) => [address]
  };

let jump_target_instruction = instr =>
  switch (Instruction.opcode(instr), Instruction.operands(instr)) {
  | (OP1_140, [Large(offset)]) =>
    let offset = signed_word(offset);
    [Instruction.jump_address(instr, offset)];
  | _ => []
  };

let all_reachable_addresses_in_routine = (story, instr_address) => {
  let immediately_reachable_addresses = address => {
    let instr = Instruction.decode(story, address);
    let following = following_instruction(instr);
    let branch = branch_target_instruction(instr);
    let jump = jump_target_instruction(instr);
    following @ branch @ jump;
  };
  reflexive_closure(instr_address, immediately_reachable_addresses);
};

let display_reachable_instructions = (story, address) => {
  let reachable = all_reachable_addresses_in_routine(story, address);
  let sorted = List.sort(compare, reachable);
  let to_string = addr => {
    let instr = Instruction.decode(story, addr);
    Instruction.display(instr, Story.version(story));
  };
  accumulate_strings(to_string, sorted);
};
