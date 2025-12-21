- \'\' enclosed chars
- Fix `&&` short-circuting not working
- enumeration in for-loops
- some sort of range-based indexing
    - OR notion of iterators like in Rust
- finish type checker
- tuple support
- remove parenthesis from while-loop cond
-  TODO: Fix
    // print(boxes);
    // boxes[i].push(p.parse_int());
    // which results in wrong behavior:
- named arguments (e.g. default = True, key = ..., etc.)
    - also: default arguments?
- Record instances should be lightweight, no hashmaps
- custom record types
    - at 'compile' time, these could just be 'compiled' into
    offsets into a tuple.
- general performance work (too many copies, bad implementations all around)
- arena alocator? ownership? borrow checker hell?
    - how does one resolve mutating e.g. `list.push(expr)`
- basic std
    - math functions (min, max)
    - where do we draw the border? what gets implemented in the interpreter vs
      in Tap itself?
- module support?
- byte code & VM
