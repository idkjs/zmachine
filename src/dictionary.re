open Utility;
open Type;

/* The table is laid out as follows. First there is a header:

   byte giving the number of word separators
   the word separators, one byte each
   byte giving the number of bytes in each dictionary entry
   word giving the number of table entries which follow

   Each entry is either 4 (in V1-3) or 6 (otherwise) bytes of zstring data,
   followed by enough bytes to make up the size of the dictionary entry. */

let word_separators_base = (Dictionary_base(base)) => Byte_address(base);

let word_separators_count = story => {
  let dict_base = Story.dictionary_base(story);
  let ws_base = word_separators_base(dict_base);
  Story.read_byte(story, ws_base);
};

let entry_base = story => {
  let dict_base = Story.dictionary_base(story);
  let ws_count = word_separators_count(story);
  let ws_base = word_separators_base(dict_base);
  inc_byte_addr_by(ws_base, ws_count + 1);
};

let entry_length = story => Story.read_byte(story, entry_base(story));

let entry_count = story => {
  let Byte_address(addr) = inc_byte_addr(entry_base(story));
  Story.read_word(story, Word_address(addr));
};

/* This is the address of the actual dictionary entries, past the initial
   header with the word separators. */
let table_base = story => {
  let Byte_address(addr) = inc_byte_addr_by(entry_base(story), 3);
  Dictionary_table_base(addr);
};

let entry_address = (story, Dictionary(dictionary_number)) => {
  let Dictionary_table_base(base) = table_base(story);
  let length = entry_length(story);
  Dictionary_address(base + dictionary_number * length);
};

let entry = (story, dictionary_number) => {
  let Dictionary_address(addr) = entry_address(story, dictionary_number);
  Zstring.read(story, Zstring(addr));
};

let display = story => {
  let count = entry_count(story);
  let to_string = i => Printf.sprintf("%s ", entry(story, Dictionary(i)));
  accumulate_strings_loop(to_string, 0, count);
};
