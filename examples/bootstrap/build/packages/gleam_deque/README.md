# deque

[![Package Version](https://img.shields.io/hexpm/v/deque)](https://hex.pm/packages/deque)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/deque/)

```sh
gleam add gleam_deque@1
```
```gleam
import gleam/deque

pub fn main() {
  let letters = 
  deque.new()
  |> deque.push_front("u")
  |> deque.push_back("c")
  |> deque.push_front("L")
  |> deque.push_back("y")


  deque.to_list(letters)
  // -> ["L", "u", "c", "y"]
}
```

Further documentation can be found at <https://hexdocs.pm/deque>.
