open Type;
open Utility;

type t = {
  original_bytes: string,
  edits: IntMap.t(char),
};

let make = bytes => {original_bytes: bytes, edits: IntMap.empty};

let size = bytes => String.length(bytes.original_bytes);

let read_byte = (bytes, address) =>
  if (is_out_of_range(address, size(bytes))) {
    failwith("address is out of range");
  } else {
    let Byte_address(addr) = address;
    let c =
      if (IntMap.mem(addr, bytes.edits)) {
        IntMap.find(addr, bytes.edits);
      } else {
        bytes.original_bytes.[addr];
      };
    int_of_char(c);
  };

let write_byte = (bytes, address, value) =>
  if (is_out_of_range(address, size(bytes))) {
    failwith("address is out of range");
  } else {
    let Byte_address(addr) = address;
    let b = char_of_int(byte_of_int(value));
    {...bytes, edits: IntMap.add(addr, b, bytes.edits)};
  };

let original = bytes => {...bytes, edits: IntMap.empty};
