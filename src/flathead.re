open Type;
open Utility;

let () = {
  let addr1 = Byte_address(1);
  let bytes_a = Immutable_bytes.make("Hello world");
  let bytes_b = Immutable_bytes.write_byte(bytes_a, addr1, 65);
  let b_a = Immutable_bytes.read_byte(bytes_a, addr1);
  let b_b = Immutable_bytes.read_byte(bytes_b, addr1);
  Printf.printf("%d %d\n", b_a, b_b);
};
