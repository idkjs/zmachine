open Utility;
open Type;

type opcode_form =
  | Long_form
  | Short_form
  | Variable_form
  | Extended_form;

type operand_count =
  | OP0
  | OP1
  | OP2
  | VAR;

let decode_variable = n => {
  let maximum_local = 15;
  if (n == 0) {
    Stack;
  } else if (n <= maximum_local) {
    Local_variable(Local(n));
  } else {
    Global_variable(Global(n));
  };
};

let encode_variable = variable =>
  switch (variable) {
  | Stack => 0
  | Local_variable(Local(n)) => n
  | Global_variable(Global(n)) => n
  };

/* We match Inform's convention of numbering the locals and globals from zero */
let display_variable = variable =>
  switch (variable) {
  | Stack => "sp"
  | Local_variable(Local(local)) => Printf.sprintf("local%d", local - 1)
  | Global_variable(Global(global)) => Printf.sprintf("g%02x", global - 16)
  };

type operand_type =
  | Large_operand
  | Small_operand
  | Variable_operand
  | Omitted;

/* The tables which follow are maps from the opcode identification number
   to the opcode type; the exact order matters. */

let one_operand_bytecodes = [|
  OP1_128,
  OP1_129,
  OP1_130,
  OP1_131,
  OP1_132,
  OP1_133,
  OP1_134,
  OP1_135,
  OP1_136,
  OP1_137,
  OP1_138,
  OP1_139,
  OP1_140,
  OP1_141,
  OP1_142,
  OP1_143,
|];

let zero_operand_bytecodes = [|
  OP0_176,
  OP0_177,
  OP0_178,
  OP0_179,
  OP0_180,
  OP0_181,
  OP0_182,
  OP0_183,
  OP0_184,
  OP0_185,
  OP0_186,
  OP0_187,
  OP0_188,
  OP0_189,
  OP0_190,
  OP0_191,
|];

let two_operand_bytecodes = [|
  ILLEGAL,
  OP2_1,
  OP2_2,
  OP2_3,
  OP2_4,
  OP2_5,
  OP2_6,
  OP2_7,
  OP2_8,
  OP2_9,
  OP2_10,
  OP2_11,
  OP2_12,
  OP2_13,
  OP2_14,
  OP2_15,
  OP2_16,
  OP2_17,
  OP2_18,
  OP2_19,
  OP2_20,
  OP2_21,
  OP2_22,
  OP2_23,
  OP2_24,
  OP2_25,
  OP2_26,
  OP2_27,
  OP2_28,
  ILLEGAL,
  ILLEGAL,
  ILLEGAL,
|];

let var_operand_bytecodes = [|
  VAR_224,
  VAR_225,
  VAR_226,
  VAR_227,
  VAR_228,
  VAR_229,
  VAR_230,
  VAR_231,
  VAR_232,
  VAR_233,
  VAR_234,
  VAR_235,
  VAR_236,
  VAR_237,
  VAR_238,
  VAR_239,
  VAR_240,
  VAR_241,
  VAR_242,
  VAR_243,
  VAR_244,
  VAR_245,
  VAR_246,
  VAR_247,
  VAR_248,
  VAR_249,
  VAR_250,
  VAR_251,
  VAR_252,
  VAR_253,
  VAR_254,
  VAR_255,
|];

let ext_bytecodes = [|
  EXT_0,
  EXT_1,
  EXT_2,
  EXT_3,
  EXT_4,
  EXT_5,
  EXT_6,
  EXT_7,
  EXT_8,
  EXT_9,
  EXT_10,
  EXT_11,
  EXT_12,
  EXT_13,
  EXT_14,
  ILLEGAL,
  EXT_16,
  EXT_17,
  EXT_18,
  EXT_19,
  EXT_20,
  EXT_21,
  EXT_22,
  EXT_23,
  EXT_24,
  EXT_25,
  EXT_26,
  EXT_27,
  EXT_28,
  EXT_29,
  ILLEGAL,
  ILLEGAL,
|];

type t = {
  opcode: bytecode,
  address: instruction_address,
  length: int,
  operands: list(operand),
  store: option(variable_location),
  branch: option((bool, branch_address)),
  text: option(string),
};

let opcode = instruction => instruction.opcode;

let address = instruction => instruction.address;

let length = instruction => instruction.length;

let operands = instruction => instruction.operands;

let store = instruction => instruction.store;

let branch = instruction => instruction.branch;

let text = instruction => instruction.text;

let following = instruction => {
  let Instruction(addr) = instruction.address;
  Instruction(addr + instruction.length);
};

let jump_address = (instruction, offset) => {
  let Instruction(addr) = instruction.address;
  Instruction(addr + instruction.length + offset - 2);
};

let is_call = (ver, opcode) =>
  switch (opcode) {
  | OP1_143 =>
    /* call_1n in v5, logical not in v1-4 */
    Story.v5_or_higher(ver)
  | VAR_224 /* call / call_vs */
  | OP1_136 /* call_1s */
  | OP2_26 /* call_2n */
  | OP2_25 /* call_2s */
  | VAR_249 /* call_vn */
  | VAR_250 /* call_vn2 */
  | VAR_236 /* call_vs2 */ => true
  | _ => false
  };

let call_address = (instr, story) =>
  if (is_call(Story.version(story), instr.opcode)) {
    switch (instr.operands) {
    | [Large(packed_address), ..._] =>
      let packed_address = Packed_routine(packed_address);
      let unpacked_address =
        Story.decode_routine_packed_address(story, packed_address);
      Some(unpacked_address);
    | _ => None
    };
  } else {
    None;
  };

let has_store = (opcode, ver) =>
  switch (opcode) {
  | OP1_143 => Story.v4_or_lower(ver) /* call_1n in v5, logical not in v1-4 */
  | OP0_181 => Story.v4_or_higher(ver) /* save branches in v3, stores in v4 */
  | OP0_182 => Story.v4_or_higher(ver) /* restore branches in v3, stores in v4 */
  | OP0_185 => Story.v4_or_higher(ver) /* pop in v4, catch in v5 */
  | VAR_233 => ver == V6
  | VAR_228 => Story.v5_or_higher(ver)
  | OP2_8
  | OP2_9
  | OP2_15
  | OP2_16
  | OP2_17
  | OP2_18
  | OP2_19
  | OP2_20
  | OP2_21
  | OP2_22
  | OP2_23
  | OP2_24
  | OP2_25
  | OP1_129
  | OP1_130
  | OP1_131
  | OP1_132
  | OP1_136
  | OP1_142
  | VAR_224
  | VAR_231
  | VAR_236
  | VAR_246
  | VAR_247
  | VAR_248
  | EXT_0
  | EXT_1
  | EXT_2
  | EXT_3
  | EXT_4
  | EXT_9
  | EXT_10
  | EXT_19
  | EXT_29 => true
  | _ => false
  };

let continues_to_following = opcode =>
  switch (opcode) {
  | OP2_28 /* throw */
  | OP1_139 /* ret */
  | OP1_140 /* jump */
  | OP0_176 /* rtrue */
  | OP0_177 /* rfalse */
  | OP0_179 /* print_ret */
  | OP0_183 /* restart */
  | OP0_184 /* ret_popped */
  | OP0_186 /* quit */ => false
  | _ => true
  };

let has_text = opcode =>
  switch (opcode) {
  | OP0_178
  | OP0_179 => true
  | _ => false
  };

let has_branch = (opcode, ver) =>
  switch (opcode) {
  | OP0_181 => Story.v3_or_lower(ver) /* save branches in v3, stores in v4 */
  | OP0_182 => Story.v3_or_lower(ver) /* restore branches in v3, stores in v4 */
  | OP2_1
  | OP2_2
  | OP2_3
  | OP2_4
  | OP2_5
  | OP2_6
  | OP2_7
  | OP2_10
  | OP1_128
  | OP1_129
  | OP1_130
  | OP0_189
  | OP0_191
  | VAR_247
  | VAR_255
  | EXT_6
  | EXT_14
  | EXT_24
  | EXT_27 => true
  | _ => false
  };

let has_indirection = (instruction, ver) =>
  switch (instruction.opcode, ver) {
  | (VAR_233, V6) => false /* pull */
  | (OP2_4, _) /* dec_chk */
  | (OP2_5, _) /* inc_chk */
  | (OP2_13, _) /* store */
  | (OP1_133, _) /* inc */
  | (OP1_134, _) /* dec */
  | (OP1_142, _) /* load */
  | (VAR_233, _) =>
    /* pull */
    true
  | _ => false
  };

let opcode_name = (opcode, ver) =>
  switch (opcode) {
  | ILLEGAL => "ILLEGAL"
  | OP2_1 => "je"
  | OP2_2 => "jl"
  | OP2_3 => "jg"
  | OP2_4 => "dec_chk"
  | OP2_5 => "inc_chk"
  | OP2_6 => "jin"
  | OP2_7 => "test"
  | OP2_8 => "or"
  | OP2_9 => "and"
  | OP2_10 => "test_attr"
  | OP2_11 => "set_attr"
  | OP2_12 => "clear_attr"
  | OP2_13 => "store"
  | OP2_14 => "insert_obj"
  | OP2_15 => "loadw"
  | OP2_16 => "loadb"
  | OP2_17 => "get_prop"
  | OP2_18 => "get_prop_addr"
  | OP2_19 => "get_next_prop"
  | OP2_20 => "add"
  | OP2_21 => "sub"
  | OP2_22 => "mul"
  | OP2_23 => "div"
  | OP2_24 => "mod"
  | OP2_25 => "call_2s"
  | OP2_26 => "call_2n"
  | OP2_27 => "set_colour"
  | OP2_28 => "throw"
  | OP1_128 => "jz"
  | OP1_129 => "get_sibling"
  | OP1_130 => "get_child"
  | OP1_131 => "get_parent"
  | OP1_132 => "get_prop_len"
  | OP1_133 => "inc"
  | OP1_134 => "dec"
  | OP1_135 => "print_addr"
  | OP1_136 => "call_1s"
  | OP1_137 => "remove_obj"
  | OP1_138 => "print_obj"
  | OP1_139 => "ret"
  | OP1_140 => "jump"
  | OP1_141 => "print_paddr"
  | OP1_142 => "load"
  | OP1_143 =>
    if (Story.v4_or_lower(ver)) {
      "not";
    } else {
      "call_1n";
    }
  | OP0_176 => "rtrue"
  | OP0_177 => "rfalse"
  | OP0_178 => "print"
  | OP0_179 => "print_ret"
  | OP0_180 => "nop"
  | OP0_181 => "save"
  | OP0_182 => "restore"
  | OP0_183 => "restart"
  | OP0_184 => "ret_popped"
  | OP0_185 =>
    if (Story.v4_or_lower(ver)) {
      "pop";
    } else {
      "catch";
    }
  | OP0_186 => "quit"
  | OP0_187 => "new_line"
  | OP0_188 => "show_status"
  | OP0_189 => "verify"
  | OP0_190 => "EXTENDED"
  | OP0_191 => "piracy"
  | VAR_224 =>
    if (Story.v3_or_lower(ver)) {
      "call";
    } else {
      "call_vs";
    }
  | VAR_225 => "storew"
  | VAR_226 => "storeb"
  | VAR_227 => "put_prop"
  | VAR_228 =>
    if (Story.v4_or_lower(ver)) {
      "sread";
    } else {
      "aread";
    }
  | VAR_229 => "print_char"
  | VAR_230 => "print_num"
  | VAR_231 => "random"
  | VAR_232 => "push"
  | VAR_233 => "pull"
  | VAR_234 => "split_window"
  | VAR_235 => "set_window"
  | VAR_236 => "call_vs2"
  | VAR_237 => "erase_window"
  | VAR_238 => "erase_line"
  | VAR_239 => "set_cursor"
  | VAR_240 => "get_cursor"
  | VAR_241 => "set_text_style"
  | VAR_242 => "buffer_mode"
  | VAR_243 => "output_stream"
  | VAR_244 => "input_stream"
  | VAR_245 => "sound_effect"
  | VAR_246 => "read_char"
  | VAR_247 => "scan_table"
  | VAR_248 => "not"
  | VAR_249 => "call_vn"
  | VAR_250 => "call_vn2"
  | VAR_251 => "tokenise"
  | VAR_252 => "encode_text"
  | VAR_253 => "copy_table"
  | VAR_254 => "print_table"
  | VAR_255 => "check_arg_count"
  | EXT_0 => "save"
  | EXT_1 => "restore"
  | EXT_2 => "log_shift"
  | EXT_3 => "art_shift"
  | EXT_4 => "set_font"
  | EXT_5 => "draw_picture"
  | EXT_6 => "picture_data"
  | EXT_7 => "erase_picture"
  | EXT_8 => "set_margins"
  | EXT_9 => "save_undo"
  | EXT_10 => "restore_undo"
  | EXT_11 => "print_unicode"
  | EXT_12 => "check_unicode"
  | EXT_13 => "set_true_colour"
  | EXT_14 => "sound_data"
  | EXT_16 => "move_window"
  | EXT_17 => "window_size"
  | EXT_18 => "window_style"
  | EXT_19 => "get_wind_prop"
  | EXT_20 => "scroll_window"
  | EXT_21 => "pop_stack"
  | EXT_22 => "read_mouse"
  | EXT_23 => "mouse_window"
  | EXT_24 => "push_stack"
  | EXT_25 => "put_wind_prop"
  | EXT_26 => "print_form"
  | EXT_27 => "make_menu"
  | EXT_28 => "picture_table"
  | EXT_29 => "buffer_screen"
  };

let display_indirect_operand = operand =>
  switch (operand) {
  | Large(large) => display_variable(decode_variable(large)) ++ " "
  | Small(small) => display_variable(decode_variable(small)) ++ " "
  | Variable(variable) => "[" ++ display_variable(variable) ++ "] "
  };

let display_operand = operand =>
  switch (operand) {
  | Large(large) => Printf.sprintf("%04x ", large)
  | Small(small) => Printf.sprintf("%02x ", small)
  | Variable(variable) => display_variable(variable) ++ " "
  };

let display_jump = instr =>
  /* For jumps, display the absolute target rather than the relative target. */
  switch (instr.operands) {
  | [Large(offset)] =>
    let offset = signed_word(offset);
    let Instruction(target) = jump_address(instr, offset);
    Printf.sprintf("%04x ", target);
  | _ => accumulate_strings(display_operand, instr.operands)
  };

let display_call = (instr, story) =>
  switch (call_address(instr, story)) {
  | Some(Routine(addr)) =>
    let routine = Printf.sprintf("%04x ", addr);
    let args = accumulate_strings(display_operand, List.tl(instr.operands));
    routine ++ args;
  | _ => accumulate_strings(display_operand, instr.operands)
  };

let display_indirect_operands = operands => {
  let var = display_indirect_operand(List.hd(operands));
  let rest = accumulate_strings(display_operand, List.tl(operands));
  var ++ rest;
};

let display = (instr, story) => {
  let ver = Story.version(story);

  let display_operands = () =>
    if (instr.opcode == OP1_140) {
      display_jump(instr);
    } else if (is_call(ver, instr.opcode)) {
      display_call(instr, story);
    } else if (has_indirection(instr, ver)) {
      display_indirect_operands(instr.operands);
    } else {
      accumulate_strings(display_operand, instr.operands);
    };

  let display_store = () =>
    switch (instr.store) {
    | None => ""
    | Some(variable) => "->" ++ display_variable(variable)
    };

  let display_branch = () =>
    switch (instr.branch) {
    | None => ""
    | Some((true, Return_false)) => "?false"
    | Some((false, Return_false)) => "?~false"
    | Some((true, Return_true)) => "?true"
    | Some((false, Return_true)) => "?~true"
    | Some((true, Branch_address(Instruction(address)))) =>
      Printf.sprintf("?%04x", address)
    | Some((false, Branch_address(Instruction(address)))) =>
      Printf.sprintf("?~%04x", address)
    };

  let display_text = () =>
    switch (instr.text) {
    | None => ""
    | Some(str) => str
    };

  let Instruction(start_addr) = instr.address;
  let name = opcode_name(instr.opcode, ver);
  let operands = display_operands();
  let store = display_store();
  let branch = display_branch();
  let text = display_text();
  Printf.sprintf(
    "%04x: %s %s%s %s %s\n",
    start_addr,
    name,
    operands,
    store,
    branch,
    text,
  );
};
/* End of display_instruction */

/* Takes the address of an instruction and produces the instruction */
let decode = (story, Instruction(address)) => {
  let addr = Byte_address(address);
  let ver = Story.version(story);
  let read_word = Story.read_word(story);
  let read_byte = Story.read_byte(story);
  let read_zstring = Zstring.read(story);
  let zstring_length = Zstring.length(story);

  /* Spec 4.3:

     Each instruction has a form (long, short, extended or variable)  ...
     If the top two bits of the opcode are $$11 the form is variable;
     if $$10, the form is short. If the opcode is 190 ($BE in hexadecimal)
     and the version is 5 or later, the form is "extended". Otherwise,
     the form is "long". */

  let decode_form = address => {
    let b = read_byte(address);
    switch (fetch_bits(bit7, size2, b)) {
    | 3 => Variable_form
    | 2 =>
      if (b == 190) {
        Extended_form;
      } else {
        Short_form;
      }
    | _ => Long_form
    };
  };

  /* Spec:
   * Each instruction has ... an operand count (0OP, 1OP, 2OP or VAR).
   * In short form, bits 4 and 5 of the opcode byte ... If this is $11
     then the operand count is 0OP; otherwise, 1OP.
   * In long form the operand count is always 2OP.
   * In variable form, if bit 5 is 0 then the count is 2OP; if it is 1,
     then the count is VAR.
   * In extended form, the operand count is VAR. */

  let decode_op_count = (address, form) => {
    let b = read_byte(address);
    switch (form) {
    | Short_form =>
      if (fetch_bits(bit5, size2, b) == 3) {
        OP0;
      } else {
        OP1;
      }
    | Long_form => OP2
    | Variable_form =>
      if (fetch_bit(bit5, b)) {
        VAR;
      } else {
        OP2;
      }
    | Extended_form => VAR
    };
  };

  /* Spec :
   * In short form, ... the opcode number is given in the bottom 4 bits.
   * In long form ... the opcode number is given in the bottom 5 bits.
   * In variable form, ... the opcode number is given in the bottom 5 bits.
   * In extended form, ... the opcode number is given in a second opcode byte. */

  /* Now what the spec does not say here clearly is: we have just read 4, 5 or
        8 bits, but we need to figure out which of 100+ opcodes we're talking
        about. The location of the bits depends on the form, but the meaning of
        of the bits depends on the operand count. In fact the operation count
        is far more relevant here. It took me some time to puzzle out this
        section of the spec. The spec could more clearly say:

      * In extended form the EXT opcode number is given in the following byte. Otherwise:
      * If the operand count is 0OP then the 0OP opcode number is given in
        the lower 4 bits.
      * If the operand count is 1OP then the 1OP opcode number is given in
        the lower 4 bits.
      * if the operand count is 2OP then the 2OP opcode number is given in
        the lower 5 bits
      * If the operand count is VAR then the VAR opcode number is given in
        the lower 5 bits
     */

  let decode_opcode = (address, form, op_count) => {
    let b = read_byte(address);
    switch (form, op_count) {
    | (Extended_form, _) =>
      let maximum_extended = 29;
      let ext = read_byte(inc_byte_addr(address));
      if (ext > maximum_extended) {
        ILLEGAL;
      } else {
        ext_bytecodes[ext];
      };
    | (_, OP0) => zero_operand_bytecodes[fetch_bits(bit3, size4, b)]
    | (_, OP1) => one_operand_bytecodes[fetch_bits(bit3, size4, b)]
    | (_, OP2) => two_operand_bytecodes[fetch_bits(bit4, size5, b)]
    | (_, VAR) => var_operand_bytecodes[fetch_bits(bit4, size5, b)]
    };
  };

  let get_opcode_length = form =>
    switch (form) {
    | Extended_form => 2
    | _ => 1
    };

  /* Spec:
     There are four 'types' of operand. These are often specified by a
     number stored in 2 binary digits:
     * $$00 Large constant (0 to 65535) 2 bytes
     * $$01 Small constant (0 to 255) 1 byte
     * $$10 Variable 1 byte
     * $$11 Omitted altogether 0 bytes */

  let decode_types = n =>
    switch (n) {
    | 0 => Large_operand
    | 1 => Small_operand
    | 2 => Variable_operand
    | _ => Omitted
    };

  /* Spec 4.4
     Next, the types of the operands are specified.
     * In short form, bits 4 and 5 of the opcode give the type.
     * In long form, bit 6 of the opcode gives the type of the first operand,
       bit 5 of the second. A value of 0 means a small constant and 1 means a
       variable.
     * In variable or extended forms, a byte of 4 operand types is given next.
       This contains 4 2-bit fields: bits 6 and 7 are the first field, bits 0 and
       1 the fourth. The values are operand types as above. Once one type has
       been given as 'omitted', all subsequent ones must be.
     * In the special case of the "double variable" VAR opcodes call_vs2 and
       call_vn2 a second byte of types is given, containing the types for the
       next four operands. */

  /* Once again this could be more clearly written; the spec never calls
        out for instance the obvious fact that 0OP codes have no operand types.
        The logic is:

     * If the count is 0OP then there are no operand types.
     * If the count is 1OP then bits 4 and 5 of the opcode give the type
     * In long form the count is 2OP; bit 6 ... */

  /* We walk the byte from low bit pairs -- which correspond to later
     operands -- to high bit pairs, so that the resulting list has
     the first operands at the head and last at the tail */
  let decode_variable_types = type_byte => {
    let rec aux = (i, acc) =>
      if (i > 3) {
        acc;
      } else {
        let type_bits = fetch_bits(Bit_number(i * 2 + 1), size2, type_byte);
        switch (decode_types(type_bits)) {
        | Omitted => aux(i + 1, acc)
        | x => aux(i + 1, [x, ...acc])
        };
      };
    aux(0, []);
  };

  let decode_operand_types = (address, form, op_count, opcode) =>
    switch (form, op_count, opcode) {
    | (_, OP0, _) => []
    | (_, OP1, _) =>
      let b = read_byte(address);
      [decode_types(fetch_bits(bit5, size2, b))];
    | (Long_form, _, _) =>
      let b = read_byte(address);
      switch (fetch_bits(bit6, size2, b)) {
      | 0 => [Small_operand, Small_operand]
      | 1 => [Small_operand, Variable_operand]
      | 2 => [Variable_operand, Small_operand]
      | _ => [Variable_operand, Variable_operand]
      };
    | (Variable_form, _, VAR_236)
    | (Variable_form, _, VAR_250) =>
      let opcode_length = get_opcode_length(form);
      let type_byte_0 = read_byte(inc_byte_addr_by(address, opcode_length));
      let type_byte_1 =
        read_byte(inc_byte_addr_by(address, opcode_length + 1));
      decode_variable_types(type_byte_0) @ decode_variable_types(type_byte_1);
    | _ =>
      let opcode_length = get_opcode_length(form);
      let type_byte = read_byte(inc_byte_addr_by(address, opcode_length));
      decode_variable_types(type_byte);
    };

  let get_type_length = (form, opcode) =>
    switch (form, opcode) {
    | (Variable_form, VAR_236)
    | (Variable_form, VAR_250) => 2
    | (Variable_form, _) => 1
    | _ => 0
    };

  /* The operand types are large, small or variable, being 2, 1 and 1 bytes
     respectively. We take the list of operand types and produce a list of
     operands. */

  /* This method is not tail recursive but the maximum number of operands
     is eight, so we don't care. */
  let rec decode_operands = (operand_address, operand_types) =>
    switch (operand_types) {
    | [] => []
    | [Large_operand, ...remaining_types] =>
      let w = read_word(byte_addr_to_word_addr(operand_address));
      let tail =
        decode_operands(
          inc_byte_addr_by(operand_address, word_size),
          remaining_types,
        );
      [Large(w), ...tail];
    | [Small_operand, ...remaining_types] =>
      let b = read_byte(operand_address);
      let tail =
        decode_operands(inc_byte_addr(operand_address), remaining_types);
      [Small(b), ...tail];
    | [Variable_operand, ...remaining_types] =>
      let b = read_byte(operand_address);
      let v = decode_variable(b);
      let tail =
        decode_operands(inc_byte_addr(operand_address), remaining_types);
      [Variable(v), ...tail];
    | [Omitted, ..._] =>
      failwith("omitted operand type passed to decode operands")
    };

  let rec get_operand_length = operand_types =>
    switch (operand_types) {
    | [] => 0
    | [Large_operand, ...remaining_types] =>
      word_size + get_operand_length(remaining_types)
    | [_, ...remaining_types] => 1 + get_operand_length(remaining_types)
    };

  /* Spec 4.6:
     "Store" instructions return a value: e.g., mul multiplies its two
     operands together. Such instructions must be followed by a single byte
     giving the variable number of where to put the result. */

  /* This is straightforward but I note something odd; the wording above
     implies that the instruction has ended after the operands, and that
     the store (and hence also branch and text) *follow* the instruction.
     I cannot get behind this. The store, branch and text are all part of
     an instruction. */

  let decode_store = (store_address, opcode, ver) =>
    if (has_store(opcode, ver)) {
      let store_byte = read_byte(store_address);
      Some(decode_variable(store_byte));
    } else {
      None;
    };

  let get_store_length = (opcode, ver) =>
    if (has_store(opcode, ver)) {
      1;
    } else {
      0;
    };

  /* Spec 4.7
   * Instructions which test a condition are called "branch" instructions.
   * The branch information is stored in one or two bytes, indicating what to
     do with the result of the test.
   * If bit 7 of the first byte is 0, a branch occurs when the condition was
     false; if 1, then branch is on true.
   * If bit 6 is set, then the branch occupies 1 byte only, and the "offset"
     is in the range 0 to 63, given in the bottom 6 bits.
   * If bit 6 is clear, then the offset is a signed 14-bit number given in
     bits 0 to 5 of the first byte followed by all 8 of the second.
   * An offset of 0 means "return false from the current routine", and 1 means
     "return true from the current routine".
   * Otherwise, a branch moves execution to the instruction at address
     (Address after branch data) + Offset - 2. */

  let decode_branch = (branch_code_address, opcode, ver) =>
    if (has_branch(opcode, ver)) {
      let high = read_byte(branch_code_address);
      let sense = fetch_bit(bit7, high);
      let bottom6 = fetch_bits(bit5, size6, high);
      let offset =
        if (fetch_bit(bit6, high)) {
          bottom6;
        } else {
          let low = read_byte(inc_byte_addr(branch_code_address));
          let unsigned = 256 * bottom6 + low;
          if (unsigned < 8192) {
            unsigned;
          } else {
            unsigned - 16384;
          };
        };
      let branch =
        switch (offset) {
        | 0 => (sense, Return_false)
        | 1 => (sense, Return_true)
        | _ =>
          let branch_length =
            if (fetch_bit(bit6, high)) {
              1;
            } else {
              2;
            };
          let Byte_address(address_after) =
            inc_byte_addr_by(branch_code_address, branch_length);
          let branch_target = Instruction(address_after + offset - 2);
          (sense, Branch_address(branch_target));
        };
      Some(branch);
    } else {
      None;
    };

  let get_branch_length = (branch_code_address, opcode, ver) =>
    if (has_branch(opcode, ver)) {
      let b = read_byte(branch_code_address);
      if (fetch_bit(bit6, b)) {
        1;
      } else {
        2;
      };
    } else {
      0;
    };

  /* Spec:
     Two opcodes, print and print_ret, are followed by a text string. */

  let decode_text = (text_address, opcode) =>
    if (has_text(opcode)) {
      Some(read_zstring(text_address));
    } else {
      None;
    };

  let get_text_length = (text_address, opcode) =>
    if (has_text(opcode)) {
      zstring_length(text_address);
    } else {
      0;
    };

  /* Helper methods are done. Start decoding */

  let form = decode_form(addr);
  let op_count = decode_op_count(addr, form);
  let opcode = decode_opcode(addr, form, op_count);
  let opcode_length = get_opcode_length(form);
  let operand_types = decode_operand_types(addr, form, op_count, opcode);
  let type_length = get_type_length(form, opcode);
  let operand_address = inc_byte_addr_by(addr, opcode_length + type_length);
  let operands = decode_operands(operand_address, operand_types);
  let operand_length = get_operand_length(operand_types);
  let store_address = inc_byte_addr_by(operand_address, operand_length);
  let store = decode_store(store_address, opcode, ver);
  let store_length = get_store_length(opcode, ver);
  let branch_code_address = inc_byte_addr_by(store_address, store_length);
  let branch = decode_branch(branch_code_address, opcode, ver);
  let branch_length = get_branch_length(branch_code_address, opcode, ver);
  let Byte_address(ba) = branch_code_address;
  let text_address = Zstring(ba + branch_length);
  let text = decode_text(text_address, opcode);
  let text_length = get_text_length(text_address, opcode);
  let length =
    opcode_length
    + type_length
    + operand_length
    + store_length
    + branch_length
    + text_length;
  let address = Instruction(address);
  {opcode, address, length, operands, store, branch, text};
};
