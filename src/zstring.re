open Utility;
open Type;

type string_state =
  | Alphabet(int)
  | Abbrev(abbreviation_number)
  | Leading
  | Trailing(int);

let abbrev0 = Abbrev(Abbreviation(0));
let abbrev32 = Abbrev(Abbreviation(32));
let abbrev64 = Abbrev(Abbreviation(64));

let alphabet0 = Alphabet(0);
let alphabet1 = Alphabet(1);
let alphabet2 = Alphabet(2);

let alphabet_table = [|
  [|
    " ",
    "?",
    "?",
    "?",
    "?",
    "?",
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z",
  |],
  [|
    " ",
    "?",
    "?",
    "?",
    "?",
    "?",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "J",
    "K",
    "L",
    "M",
    "N",
    "O",
    "P",
    "Q",
    "R",
    "S",
    "T",
    "U",
    "V",
    "W",
    "X",
    "Y",
    "Z",
  |],
  [|
    " ",
    "?",
    "?",
    "?",
    "?",
    "?",
    "?",
    "\n",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    ".",
    ",",
    "!",
    "?",
    "_",
    "#",
    "'",
    "\"",
    "/",
    "\\",
    "-",
    ":",
    "(",
    ")",
  |],
|];

let abbreviation_table_length = 96;

/* A "word address" is only used in the abbreviation table, and is always
   just half the real address. A "packed address" is used in calls and fetching
   strings, and is half the real address in v3 but different for other versions. */

let decode_word_address = (Word_zstring(word_address)) =>
  Zstring(word_address * 2);

let first_abbrev_addr = (Abbreviation_table_base(base)) =>
  Word_address(base);

let abbreviation_zstring = (story, Abbreviation(n)) =>
  if (n < 0 || n >= abbreviation_table_length) {
    failwith("bad offset into abbreviation table");
  } else {
    let base = first_abbrev_addr(Story.abbreviations_table_base(story));
    let abbr_addr = inc_word_addr_by(base, n);
    let word_addr = Word_zstring(Story.read_word(story, abbr_addr));
    decode_word_address(word_addr);
  };

let rec read = (story, Zstring(address)) => {
  /* TODO: Only processes version 3 strings */

  /* zstrings encode three characters into two-byte words.

     The high bit is the end-of-string marker, followed by three
     five-bit zchars.

     The meaning of the next zchar(s) depends on the current.

     If the current zchar is 1, 2 or 3 then the next is an offset
     into the abbreviation table; fetch the string indicated there.

     If the current zchar is 4 or 5 then the next is an offset into the
     uppercase or punctuation alphabets, except if the current is 5
     and the next is 6. In that case the two zchars following are a single
     10-bit character. */

  let process_zchar = (Zchar(zchar), state) =>
    switch (zchar, state) {
    | (1, Alphabet(_)) => ("", abbrev0)
    | (2, Alphabet(_)) => ("", abbrev32)
    | (3, Alphabet(_)) => ("", abbrev64)
    | (4, Alphabet(_)) => ("", alphabet1)
    | (5, Alphabet(_)) => ("", alphabet2)
    | (6, Alphabet(2)) => ("", Leading)
    | (_, Alphabet(a)) => (alphabet_table[a][zchar], alphabet0)
    | (_, Abbrev(Abbreviation(a))) =>
      let abbrv = Abbreviation(a + zchar);
      let addr = abbreviation_zstring(story, abbrv);
      let str = read(story, addr);
      (str, alphabet0);
    | (_, Leading) => ("", Trailing(zchar))
    | (_, Trailing(high)) =>
      let s = string_of_char(Char.chr(high * 32 + zchar));
      (s, alphabet0);
    };

  let rec aux = (acc, state1, current_address) => {
    let zchar_bit_size = size5;
    let word = Story.read_word(story, current_address);
    let is_end = fetch_bit(bit15, word);
    let zchar1 = Zchar(fetch_bits(bit14, zchar_bit_size, word));
    let zchar2 = Zchar(fetch_bits(bit9, zchar_bit_size, word));
    let zchar3 = Zchar(fetch_bits(bit4, zchar_bit_size, word));
    let (text1, state2) = process_zchar(zchar1, state1);
    let (text2, state3) = process_zchar(zchar2, state2);
    let (text3, state_next) = process_zchar(zchar3, state3);
    let new_acc = acc ++ text1 ++ text2 ++ text3;
    if (is_end) {
      new_acc;
    } else {
      aux(new_acc, state_next, inc_word_addr(current_address));
    };
  };
  aux("", alphabet0, Word_address(address));
};

/* A debugging method for looking at memory broken up into the
   1 / 5 / 5 / 5 bit chunks used by zstrings. */

let display_bytes = (story, Zstring(addr)) => {
  let rec aux = (current, acc) => {
    let word = Story.read_word(story, current);
    let is_end = fetch_bits(bit15, size1, word);
    let zchar1 = fetch_bits(bit14, size5, word);
    let zchar2 = fetch_bits(bit9, size5, word);
    let zchar3 = fetch_bits(bit4, size5, word);
    let s = Printf.sprintf("%02x %02x %02x ", zchar1, zchar2, zchar3);
    let acc = acc ++ s;
    if (is_end == 1) {
      acc;
    } else {
      aux(inc_word_addr(current), acc);
    };
  };
  aux(Word_address(addr), "");
};

/* gives the length in bytes of the encoded zstring, not the decoded string */
let length = (story, Zstring(address)) => {
  let rec aux = (len, current) =>
    if (fetch_bit(bit15, Story.read_word(story, current))) {
      len + 2;
    } else {
      aux(len + 2, inc_word_addr(current));
    };
  aux(0, Word_address(address));
};
