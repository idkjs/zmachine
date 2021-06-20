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
type zchar =
  | Zchar(int);
type dictionary_table_base =
  | Dictionary_table_base(int);
type dictionary_base =
  | Dictionary_base(int);
type dictionary_address =
  | Dictionary_address(int);
type dictionary_number =
  | Dictionary(int);
type word_separator_number =
  | Word_separator_number(int);
type object_base =
  | Object_base(int);
type property_defaults_table =
  | Property_defaults_table(int);
type object_tree_base =
  | Object_tree_base(int);
type object_number =
  | Object(int);
type object_address =
  | Object_address(int);
type property_header_address =
  | Property_header(int);

type version =
  | V1
  | V2
  | V3
  | V4
  | V5
  | V6
  | V7
  | V8;
