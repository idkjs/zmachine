module IntMap =
  Map.Make({
    type t = int;
    let compare = compare;
  });
type bit_number =
  | Bit_number(int);
type bit_size =
  | Bit_size(int);
type byte_address =
  | Byte_address(int);
type word_address =
  | Word_address(int);
type abbreviation_number =
  | Abbreviation(int);
type abbrev_table_base =
  | Abbreviation_table_base(int);
type word_zstring_address =
  | Word_zstring(int);
type zstring_address =
  | Zstring(int);
