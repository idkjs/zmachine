open Type;

let bit0 = Bit_number(0);
let bit1 = Bit_number(1);
let bit2 = Bit_number(2);
let bit3 = Bit_number(3);
let bit4 = Bit_number(4);
let bit5 = Bit_number(5);
let bit6 = Bit_number(6);
let bit7 = Bit_number(7);
let bit8 = Bit_number(8);
let bit9 = Bit_number(9);
let bit10 = Bit_number(10);
let bit11 = Bit_number(11);
let bit12 = Bit_number(12);
let bit13 = Bit_number(13);
let bit14 = Bit_number(14);
let bit15 = Bit_number(15);

let size1 = Bit_size(1);
let size2 = Bit_size(2);
let size3 = Bit_size(3);
let size4 = Bit_size(4);
let size5 = Bit_size(5);
let size6 = Bit_size(6);
let size7 = Bit_size(7);

let fetch_bit = (Bit_number(n), word) => (word land 1 lsl n) lsr n == 1;

let clear_bit = (Bit_number(n), word) => word land lnot(1 lsl n);

let set_bit = (Bit_number(n), word) => word lor 1 lsl n;

let set_bit_to = (n, word, value) =>
  if (value) {
    set_bit(n, word);
  } else {
    clear_bit(n, word);
  };

let fetch_bits = (Bit_number(high), Bit_size(length), word) => {
  let mask = lnot((-1) lsl length);
  word lsr (high - length + 1) land mask;
};

let byte_of_int = x => x land 0xff;

let is_in_range = (Byte_address(address), size) =>
  0 <= address && address < size;

let is_out_of_range = (address, size) => !is_in_range(address, size);
