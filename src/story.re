open Utility;
open Type;

type t = {
  dynamic_memory: Immutable_bytes.t,
  static_memory: string,
};

let make = (dynamic, static) => {
  dynamic_memory: Immutable_bytes.make(dynamic),
  static_memory: static,
};

let read_byte = (story, address) => {
  let dynamic_size = Immutable_bytes.size(story.dynamic_memory);
  if (is_in_range(address, dynamic_size)) {
    Immutable_bytes.read_byte(story.dynamic_memory, address);
  } else {
    let static_addr = dec_byte_addr_by(address, dynamic_size);
    dereference_string(static_addr, story.static_memory);
  };
};

let read_word = (story, address) => {
  let high = read_byte(story, address_of_high_byte(address));
  let low = read_byte(story, address_of_low_byte(address));
  256 * high + low;
};

let write_byte = (story, address, value) => {
  let dynamic_memory =
    Immutable_bytes.write_byte(story.dynamic_memory, address, value);
  {...story, dynamic_memory};
};

let write_word = (story, address, value) => {
  let high = value lsr 8 land 0xFF;
  let low = value land 0xFF;
  let story = write_byte(story, address_of_high_byte(address), high);
  write_byte(story, address_of_low_byte(address), low);
};

let read_bit = (story, address, bit) =>
  fetch_bit(bit, read_byte(story, address));

let read_word_bit = (story, address, bit) =>
  fetch_bit(bit, read_word(story, address));

let write_set_bit = (story, address, bit) => {
  let orig_byte = read_byte(story, address);
  let new_byte = set_bit(bit, orig_byte);
  write_byte(story, address, new_byte);
};

let write_clear_bit = (story, address, bit) => {
  let orig_byte = read_byte(story, address);
  let new_byte = clear_bit(bit, orig_byte);
  write_byte(story, address, new_byte);
};

let write_set_bit_to = (story, address, bit, value) => {
  let orig_byte = read_byte(story, address);
  let new_byte = set_bit_to(bit, orig_byte, value);
  write_byte(story, address, new_byte);
};

let write_set_word_bit_to = (story, address, bit, value) => {
  let orig_word = read_word(story, address);
  let new_word = set_bit_to(bit, orig_word, value);
  write_word(story, address, new_word);
};

let header_size = 64;

let version_offset = Byte_address(0);
let version = story =>
  switch (read_byte(story, version_offset)) {
  | 1 => V1
  | 2 => V2
  | 3 => V3
  | 4 => V4
  | 5 => V5
  | 6 => V6
  | 7 => V7
  | 8 => V8
  | _ => failwith("unknown version")
  };

let v5_or_lower = v =>
  switch (v) {
  | V1
  | V2
  | V3
  | V4
  | V5 => true
  | V6
  | V7
  | V8 => false
  };

let v6_or_higher = v => !v5_or_lower(v);

let v4_or_lower = v =>
  switch (v) {
  | V1
  | V2
  | V3
  | V4 => true
  | V5
  | V6
  | V7
  | V8 => false
  };

let v5_or_higher = v => !v4_or_lower(v);

let v3_or_lower = v =>
  switch (v) {
  | V1
  | V2
  | V3 => true
  | V4
  | V5
  | V6
  | V7
  | V8 => false
  };

let v4_or_higher = v => !v3_or_lower(v);

/* Bytes 6 and 7 are the initial pc for the main routine. In version 6 (only)
   this is the (packed!) address of a routine, so the *following* byte is the first
   instruction. In all other versions the main routine is indicated just by
   its first instruction. */

let initial_program_counter = story => {
  let initial_program_counter_offset = Word_address(6);
  let pc = read_word(story, initial_program_counter_offset);
  if (version(story) == V6) {
    Instruction(pc * 4 + 1);
  } else {
    Instruction(pc);
  };
};

let dictionary_base = story => {
  let dictionary_base_offset = Word_address(8);
  Dictionary_base(read_word(story, dictionary_base_offset));
};

let object_table_base = story => {
  let object_table_base_offset = Word_address(10);
  Object_base(read_word(story, object_table_base_offset));
};

let global_variables_table_base = story => {
  let global_variables_table_base_offset = Word_address(12);
  Global_table_base(read_word(story, global_variables_table_base_offset));
};

let static_memory_base_offset = Word_address(14);

let abbreviations_table_base = story => {
  let abbreviations_table_base_offset = Word_address(24);
  Abbreviation_table_base(read_word(story, abbreviations_table_base_offset));
};

let routine_offset = story => {
  let routine_offset_offset = Word_address(40);
  8 * read_word(story, routine_offset_offset);
};

let string_offset = story => {
  let string_offset_offset = Word_address(42);
  8 * read_word(story, string_offset_offset);
};

let load = filename => {
  let file = get_file(filename);
  let len = String.length(file);
  if (len < header_size) {
    failwith(Printf.sprintf("%s is not a valid story file", filename));
  } else {
    let high =
      dereference_string(
        address_of_high_byte(static_memory_base_offset),
        file,
      );
    let low =
      dereference_string(
        address_of_low_byte(static_memory_base_offset),
        file,
      );
    let dynamic_length = high * 256 + low;
    if (dynamic_length > len) {
      failwith(Printf.sprintf("%s is not a valid story file", filename));
    } else {
      let dynamic = String.sub(file, 0, dynamic_length);
      let static = String.sub(file, dynamic_length, len - dynamic_length);
      make(dynamic, static);
    };
  };
};

let decode_routine_packed_address = (story, Packed_routine(packed)) =>
  switch (version(story)) {
  | V1
  | V2
  | V3 => Routine(packed * 2)
  | V4
  | V5 => Routine(packed * 4)
  | V6
  | V7 => Routine(packed * 4 + routine_offset(story))
  | V8 => Routine(packed * 8)
  };

let decode_string_packed_address = (story, Packed_zstring(packed)) =>
  switch (version(story)) {
  | V1
  | V2
  | V3 => Zstring(packed * 2)
  | V4
  | V5 => Zstring(packed * 4)
  | V6
  | V7 => Zstring(packed * 4 + string_offset(story))
  | V8 => Zstring(packed * 8)
  };
