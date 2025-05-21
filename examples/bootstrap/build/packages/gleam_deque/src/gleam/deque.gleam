import gleam/list

/// A deque (double-ended-queue) is an ordered collection of elements. It is
/// similar to a list.
/// Unlike a list elements can be added to or removed from either the front or
/// the back in a performant fashion.
///
/// The internal representation may be different for two deques with the same
/// elements in the same order if the deques were constructed in different
/// ways. This is the price paid for a deque's fast access at both the front
/// and the back.
///
/// Because of unpredictable internal representation the equality operator `==`
/// may return surprising results, and the `is_equal` and `is_logically_equal`
/// functions are the recommended way to test deques for equality.
///
pub opaque type Deque(a) {
  Deque(in: List(a), out: List(a))
}

/// Creates a fresh deque that contains no values.
///
pub fn new() -> Deque(a) {
  Deque(in: [], out: [])
}

/// Converts a list of elements into a deque of the same elements in the same
/// order. The first element in the list becomes the front element in the deque.
///
/// This function runs in constant time.
///
/// # Examples
///
/// ```gleam
/// [1, 2, 3] |> from_list |> length
/// // -> 3
/// ```
///
pub fn from_list(list: List(a)) -> Deque(a) {
  Deque(in: [], out: list)
}

/// Converts a deque of elements into a list of the same elements in the same
/// order. The front element in the deque becomes the first element in the list.
///
/// This function runs in linear time.
///
/// # Examples
///
/// ```gleam
/// new() |> push_back(1) |> push_back(2) |> to_list
/// // -> [1, 2]
/// ```
///
pub fn to_list(deque: Deque(a)) -> List(a) {
  deque.out
  |> list.append(list.reverse(deque.in))
}

/// Determines whether or not the deque is empty.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// [] |> from_list |> is_empty
/// // -> True
/// ```
///
/// ```gleam
/// [1] |> from_list |> is_empty
/// // -> False
/// ```
///
/// ```gleam
/// [1, 2] |> from_list |> is_empty
/// // -> False
/// ```
///
pub fn is_empty(deque: Deque(a)) -> Bool {
  deque.in == [] && deque.out == []
}

/// Counts the number of elements in a given deque.
///
/// This function has to traverse the deque to determine the number of elements,
/// so it runs in linear time.
///
/// ## Examples
///
/// ```gleam
/// length(from_list([]))
/// // -> 0
/// ```
///
/// ```gleam
/// length(from_list([1]))
/// // -> 1
/// ```
///
/// ```gleam
/// length(from_list([1, 2]))
/// // -> 2
/// ```
///
pub fn length(deque: Deque(a)) -> Int {
  list.length(deque.in) + list.length(deque.out)
}

/// Pushes an element onto the back of the deque.
///
/// # Examples
///
/// ```gleam
/// [1, 2] |> from_list |> push_back(3) |> to_list
/// // -> [1, 2, 3]
/// ```
///
pub fn push_back(onto deque: Deque(a), this item: a) -> Deque(a) {
  Deque(in: [item, ..deque.in], out: deque.out)
}

/// Pushes an element onto the front of the deque.
///
/// # Examples
///
/// ```gleam
/// [0, 0] |> from_list |> push_front(1) |> to_list
/// // -> [1, 0, 0]
/// ```
///
pub fn push_front(onto deque: Deque(a), this item: a) -> Deque(a) {
  Deque(in: deque.in, out: [item, ..deque.out])
}

/// Gets the last element from the deque, returning the
/// element and a new deque without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
/// ```gleam
/// new()
/// |> push_back(0)
/// |> push_back(1)
/// |> pop_back
/// // -> Ok(#(1, push_front(new(), 0)))
/// ```
///
/// ```gleam
/// new()
/// |> push_front(0)
/// |> pop_back
/// // -> Ok(#(0, new()))
/// ```
///
/// ```gleam
/// new() |> pop_back
/// // -> Error(Nil)
/// ```
///
pub fn pop_back(from deque: Deque(a)) -> Result(#(a, Deque(a)), Nil) {
  case deque {
    Deque(in: [], out: []) -> Error(Nil)
    Deque(in: [], out: out) -> pop_back(Deque(in: list.reverse(out), out: []))
    Deque(in: [first, ..rest], out: out) -> {
      let deque = Deque(in: rest, out: out)
      Ok(#(first, deque))
    }
  }
}

/// Gets the first element from the deque, returning the
/// element and a new deque without that element.
///
/// This function typically runs in constant time, but will occasionally run in
/// linear time.
///
/// # Examples
///
/// ```gleam
/// new()
/// |> push_front(1)
/// |> push_front(0)
/// |> pop_front
/// // -> Ok(#(0, push_back(new(), 1)))
/// ```
///
/// ```gleam
/// new()
/// |> push_back(0)
/// |> pop_front
/// // -> Ok(#(0, new()))
/// ```
///
/// ```gleam
/// new() |> pop_back
/// // -> Error(Nil)
/// ```
///
pub fn pop_front(from deque: Deque(a)) -> Result(#(a, Deque(a)), Nil) {
  case deque {
    Deque(in: [], out: []) -> Error(Nil)
    Deque(in: in, out: []) -> pop_front(Deque(in: [], out: list.reverse(in)))
    Deque(in: in, out: [first, ..rest]) -> {
      let deque = Deque(in: in, out: rest)
      Ok(#(first, deque))
    }
  }
}

/// Creates a new deque from a given deque containing the same elements, but in
/// the opposite order.
///
/// This function runs in constant time.
///
/// ## Examples
///
/// ```gleam
/// [] |> from_list |> reverse |> to_list
/// // -> []
/// ```
///
/// ```gleam
/// [1] |> from_list |> reverse |> to_list
/// // -> [1]
/// ```
///
/// ```gleam
/// [1, 2] |> from_list |> reverse |> to_list
/// // -> [2, 1]
/// ```
///
pub fn reverse(deque: Deque(a)) -> Deque(a) {
  Deque(in: deque.out, out: deque.in)
}

/// Checks whether two deques have equal elements in the same order, where the
/// equality of elements is determined by a given equality checking function.
///
/// This function is useful as the internal representation may be different for
/// two deques with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time multiplied by the time taken by the
/// element equality checking function.
///
pub fn is_logically_equal(
  a: Deque(a),
  to b: Deque(a),
  checking element_is_equal: fn(a, a) -> Bool,
) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, element_is_equal)
}

fn check_equal(
  xs: List(a),
  x_tail: List(a),
  ys: List(a),
  y_tail: List(a),
  eq: fn(a, a) -> Bool,
) -> Bool {
  case xs, x_tail, ys, y_tail {
    [], [], [], [] -> True
    [x, ..xs], _, [y, ..ys], _ ->
      case eq(x, y) {
        False -> False
        True -> check_equal(xs, x_tail, ys, y_tail, eq)
      }
    [], [_, ..], _, _ -> check_equal(list.reverse(x_tail), [], ys, y_tail, eq)
    _, _, [], [_, ..] -> check_equal(xs, x_tail, list.reverse(y_tail), [], eq)
    _, _, _, _ -> False
  }
}

/// Checks whether two deques have the same elements in the same order.
///
/// This function is useful as the internal representation may be different for
/// two deques with the same elements in the same order depending on how they
/// were constructed, so the equality operator `==` may return surprising
/// results.
///
/// This function runs in linear time.
///
pub fn is_equal(a: Deque(a), to b: Deque(a)) -> Bool {
  check_equal(a.out, a.in, b.out, b.in, fn(a, b) { a == b })
}
