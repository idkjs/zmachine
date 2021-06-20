open Utility;
open Type;

/* The object table is laid out as follows:

   * The base of the object table is in the header.
   * The object table begins with a block of 31 or 63 default property values.
   * Following the default property values is the object tree.
   * Each entry in the tree is of the same size, and is laid out as follows:
     * 32 or 48 bits of attribute flags
     * the parent, sibling and child object numbers
     * the address of an additional table of variable-sized properties.
   * object numbers are one-based, so zero is used as the invalid object.
   */

let invalid_object = Object(0);

let default_property_table_size = story =>
  if (Story.v3_or_lower(Story.version(story))) {
    31;
  } else {
    63;
  };

let default_property_table_entry_size = 2;

let tree_base = story => {
  let Object_base(base) = Story.object_table_base(story);
  let table_size = default_property_table_size(story);
  Object_tree_base(base + default_property_table_entry_size * table_size);
};

let entry_size = story =>
  if (Story.v3_or_lower(Story.version(story))) {
    9;
  } else {
    14;
  };

let address = (story, Object(obj)) => {
  let Object_tree_base(tree_base) = tree_base(story);
  let entry_size = entry_size(story);
  Object_address(tree_base + (obj - 1) * entry_size);
};

let parent = (story, obj) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Object(Story.read_byte(story, Byte_address(addr + 4)));
  } else {
    Object(Story.read_word(story, Word_address(addr + 6)));
  };
};

let set_parent = (story, obj, Object(new_parent)) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Story.write_byte(story, Byte_address(addr + 4), new_parent);
  } else {
    Story.write_word(story, Word_address(addr + 6), new_parent);
  };
};

let sibling = (story, obj) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Object(Story.read_byte(story, Byte_address(addr + 5)));
  } else {
    Object(Story.read_word(story, Word_address(addr + 8)));
  };
};

let set_sibling = (story, obj, Object(new_sibling)) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Story.write_byte(story, Byte_address(addr + 5), new_sibling);
  } else {
    Story.write_word(story, Word_address(addr + 8), new_sibling);
  };
};

let child = (story, obj) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Object(Story.read_byte(story, Byte_address(addr + 6)));
  } else {
    Object(Story.read_word(story, Word_address(addr + 10)));
  };
};

let set_child = (story, obj, Object(new_child)) => {
  let Object_address(addr) = address(story, obj);
  if (Story.v3_or_lower(Story.version(story))) {
    Story.write_byte(story, Byte_address(addr + 6), new_child);
  } else {
    Story.write_word(story, Word_address(addr + 10), new_child);
  };
};

/* The last two bytes in an object description are a pointer to a
   block that contains additional properties. */
let property_header_address = (story, obj) => {
  let object_property_offset =
    if (Story.v3_or_lower(Story.version(story))) {
      7;
    } else {
      12;
    };
  let Object_address(addr) = address(story, obj);
  Property_header(
    Story.read_word(story, Word_address(addr + object_property_offset)),
  );
};

/* Oddly enough, the Z machine does not ever say how big the object table is.
   Assume that the address of the first property block in the first object is
   the bottom of the object tree table. */
let count = story => {
  let Object_tree_base(table_start) = tree_base(story);
  let Property_header(table_end) =
    property_header_address(story, Object(1));
  let entry_size = entry_size(story);
  (table_end - table_start) / entry_size;
};

/* The property entry begins with a length-prefixed zstring */
let name = (story, n) => {
  let Property_header(addr) = property_header_address(story, n);
  let length = Story.read_byte(story, Byte_address(addr));
  if (length == 0) {
    "<unnamed>";
  } else {
    Zstring.read(story, Zstring(addr + 1));
  };
};

let find_previous_sibling = (story, obj) => {
  let rec aux = current => {
    let next_sibling = sibling(story, current);
    if (next_sibling == obj) {
      current;
    } else {
      aux(next_sibling);
    };
  };
  let parent = parent(story, obj);
  let first_child = child(story, parent);
  aux(first_child);
};

/* Takes a child object and detatches it from its parent */

let remove = (story, obj) => {
  let original_parent = parent(story, obj);
  if (original_parent == invalid_object) {
    story;
  } else {
    /* First edit: if the child is the parent's first child then
       make the next sibling the new first child.  If the child
       is not the first child then the previous sibling
       needs to point to the next sibling. */
    /* Already detatched */

    let edit1 = {
      let sibling = sibling(story, obj);
      if (obj == child(story, original_parent)) {
        set_child(story, original_parent, sibling);
      } else {
        let prev_sibling = find_previous_sibling(story, obj);
        set_sibling(story, prev_sibling, sibling);
      };
    };
    /* Second edit: the child now has no parent. */
    set_parent(edit1, obj, invalid_object);
  };
};

/* Takes a child object and a parent object, and causes the child to be the
   first child of the parent. */
let insert = (story, new_child, new_parent) => {
  /* Detatch the new child from its old parent */
  let edit1 = remove(story, new_child);
  /* Hook up the new child to its new parent */
  let edit2 = set_parent(edit1, new_child, new_parent);
  /* Hook up the sibling chain */
  let edit3 = set_sibling(edit2, new_child, child(edit2, new_parent));
  /* Make the child the new first child of the parent */
  set_child(edit3, new_parent, new_child);
};

let display_object_table = story => {
  let count = count(story);
  let to_string = i => {
    let current = Object(i);
    let Object(parent) = parent(story, current);
    let Object(sibling) = sibling(story, current);
    let Object(child) = child(story, current);
    let name = name(story, current);
    Printf.sprintf(
      "%02x: %02x %02x %02x %s\n",
      i,
      parent,
      sibling,
      child,
      name,
    );
  };
  accumulate_strings_loop(to_string, 1, count + 1);
};

/* Count down all the objects in the object table and record which ones have no parent. */
let roots = story => {
  let rec aux = (obj, acc) => {
    let current = Object(obj);
    if (current == invalid_object) {
      acc;
    } else if (parent(story, current) == invalid_object) {
      aux(obj - 1, [current, ...acc]);
    } else {
      aux(obj - 1, acc);
    };
  };
  aux(count(story), []);
};

let display_object_tree = story => {
  let rec aux = (acc, indent, obj) =>
    if (obj == invalid_object) {
      acc;
    } else {
      let name = name(story, obj);
      let child = child(story, obj);
      let sibling = sibling(story, obj);
      let object_text = Printf.sprintf("%s%s\n", indent, name);
      let with_object = acc ++ object_text;
      let new_indent = "    " ++ indent;
      let with_children = aux(with_object, new_indent, child);
      aux(with_children, indent, sibling);
    };
  let to_string = obj => aux("", "", obj);
  accumulate_strings(to_string, roots(story));
};
