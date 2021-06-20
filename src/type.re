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
