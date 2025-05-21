import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $order from "../../gleam_stdlib/gleam/order.mjs";
import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

class Stop extends $CustomType {}

class Continue extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class Yielder extends $CustomType {
  constructor(continuation) {
    super();
    this.continuation = continuation;
  }
}

export class Next extends $CustomType {
  constructor(element, accumulator) {
    super();
    this.element = element;
    this.accumulator = accumulator;
  }
}

export class Done extends $CustomType {}

class AnotherBy extends $CustomType {
  constructor(x0, x1, x2, x3) {
    super();
    this[0] = x0;
    this[1] = x1;
    this[2] = x2;
    this[3] = x3;
  }
}

class LastBy extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class Another extends $CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
}

class Last extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class NoMore extends $CustomType {}

function stop() {
  return new Stop();
}

function unfold_loop(initial, f) {
  return () => {
    let $ = f(initial);
    if ($ instanceof Next) {
      let x = $.element;
      let acc = $.accumulator;
      return new Continue(x, unfold_loop(acc, f));
    } else {
      return new Stop();
    }
  };
}

export function unfold(initial, f) {
  let _pipe = initial;
  let _pipe$1 = unfold_loop(_pipe, f);
  return new Yielder(_pipe$1);
}

export function repeatedly(f) {
  return unfold(undefined, (_) => { return new Next(f(), undefined); });
}

export function repeat(x) {
  return repeatedly(() => { return x; });
}

export function from_list(list) {
  let yield$1 = (acc) => {
    if (acc.hasLength(0)) {
      return new Done();
    } else {
      let head = acc.head;
      let tail = acc.tail;
      return new Next(head, tail);
    }
  };
  return unfold(list, yield$1);
}

function transform_loop(continuation, state, f) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let el = $[0];
      let next = $[1];
      let $1 = f(state, el);
      if ($1 instanceof Done) {
        return new Stop();
      } else {
        let yield$1 = $1.element;
        let next_state = $1.accumulator;
        return new Continue(yield$1, transform_loop(next, next_state, f));
      }
    }
  };
}

export function transform(yielder, initial, f) {
  let _pipe = transform_loop(yielder.continuation, initial, f);
  return new Yielder(_pipe);
}

function fold_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Continue) {
      let elem = $[0];
      let next = $[1];
      loop$continuation = next;
      loop$f = f;
      loop$accumulator = f(accumulator, elem);
    } else {
      return accumulator;
    }
  }
}

export function fold(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return fold_loop(_pipe, f, initial);
}

export function run(yielder) {
  return fold(yielder, undefined, (_, _1) => { return undefined; });
}

export function to_list(yielder) {
  let _pipe = yielder;
  let _pipe$1 = fold(
    _pipe,
    toList([]),
    (acc, e) => { return listPrepend(e, acc); },
  );
  return $list.reverse(_pipe$1);
}

export function step(yielder) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Done();
  } else {
    let e = $[0];
    let a = $[1];
    return new Next(e, new Yielder(a));
  }
}

function take_loop(continuation, desired) {
  return () => {
    let $ = desired > 0;
    if (!$) {
      return new Stop();
    } else {
      let $1 = continuation();
      if ($1 instanceof Stop) {
        return new Stop();
      } else {
        let e = $1[0];
        let next = $1[1];
        return new Continue(e, take_loop(next, desired - 1));
      }
    }
  };
}

export function take(yielder, desired) {
  let _pipe = yielder.continuation;
  let _pipe$1 = take_loop(_pipe, desired);
  return new Yielder(_pipe$1);
}

function drop_loop(loop$continuation, loop$desired) {
  while (true) {
    let continuation = loop$continuation;
    let desired = loop$desired;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = desired > 0;
      if ($1) {
        loop$continuation = next;
        loop$desired = desired - 1;
      } else {
        return new Continue(e, next);
      }
    }
  }
}

export function drop(yielder, desired) {
  let _pipe = () => { return drop_loop(yielder.continuation, desired); };
  return new Yielder(_pipe);
}

function map_loop(continuation, f) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue(f(e), map_loop(continuation$1, f));
    }
  };
}

export function map(yielder, f) {
  let _pipe = yielder.continuation;
  let _pipe$1 = map_loop(_pipe, f);
  return new Yielder(_pipe$1);
}

function map2_loop(continuation1, continuation2, fun) {
  return () => {
    let $ = continuation1();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let a = $[0];
      let next_a = $[1];
      let $1 = continuation2();
      if ($1 instanceof Stop) {
        return new Stop();
      } else {
        let b = $1[0];
        let next_b = $1[1];
        return new Continue(fun(a, b), map2_loop(next_a, next_b, fun));
      }
    }
  };
}

export function map2(yielder1, yielder2, fun) {
  let _pipe = map2_loop(yielder1.continuation, yielder2.continuation, fun);
  return new Yielder(_pipe);
}

function append_loop(first, second) {
  let $ = first();
  if ($ instanceof Continue) {
    let e = $[0];
    let first$1 = $[1];
    return new Continue(e, () => { return append_loop(first$1, second); });
  } else {
    return second();
  }
}

export function append(first, second) {
  let _pipe = () => {
    return append_loop(first.continuation, second.continuation);
  };
  return new Yielder(_pipe);
}

function flatten_loop(flattened) {
  let $ = flattened();
  if ($ instanceof Stop) {
    return new Stop();
  } else {
    let it = $[0];
    let next_yielder = $[1];
    return append_loop(
      it.continuation,
      () => { return flatten_loop(next_yielder); },
    );
  }
}

export function flatten(yielder) {
  let _pipe = () => { return flatten_loop(yielder.continuation); };
  return new Yielder(_pipe);
}

export function concat(yielders) {
  return flatten(from_list(yielders));
}

export function flat_map(yielder, f) {
  let _pipe = yielder;
  let _pipe$1 = map(_pipe, f);
  return flatten(_pipe$1);
}

function filter_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let yielder = $[1];
      let $1 = predicate(e);
      if ($1) {
        return new Continue(
          e,
          () => { return filter_loop(yielder, predicate); },
        );
      } else {
        loop$continuation = yielder;
        loop$predicate = predicate;
      }
    }
  }
}

export function filter(yielder, predicate) {
  let _pipe = () => { return filter_loop(yielder.continuation, predicate); };
  return new Yielder(_pipe);
}

function filter_map_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1.isOk()) {
        let e$1 = $1[0];
        return new Continue(e$1, () => { return filter_map_loop(next, f); });
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

export function filter_map(yielder, f) {
  let _pipe = () => { return filter_map_loop(yielder.continuation, f); };
  return new Yielder(_pipe);
}

export function cycle(yielder) {
  let _pipe = repeat(yielder);
  return flatten(_pipe);
}

function find_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Error(undefined);
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1) {
        return new Ok(e);
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

export function find(haystack, is_desired) {
  let _pipe = haystack.continuation;
  return find_loop(_pipe, is_desired);
}

function find_map_loop(loop$continuation, loop$f) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Error(undefined);
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = f(e);
      if ($1.isOk()) {
        let e$1 = $1[0];
        return new Ok(e$1);
      } else {
        loop$continuation = next;
        loop$f = f;
      }
    }
  }
}

export function find_map(haystack, is_desired) {
  let _pipe = haystack.continuation;
  return find_map_loop(_pipe, is_desired);
}

function index_loop(continuation, next) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let continuation$1 = $[1];
      return new Continue([e, next], index_loop(continuation$1, next + 1));
    }
  };
}

export function index(yielder) {
  let _pipe = yielder.continuation;
  let _pipe$1 = index_loop(_pipe, 0);
  return new Yielder(_pipe$1);
}

export function iterate(initial, f) {
  return unfold(initial, (element) => { return new Next(element, f(element)); });
}

function take_while_loop(continuation, predicate) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if (!$1) {
        return new Stop();
      } else {
        return new Continue(e, take_while_loop(next, predicate));
      }
    }
  };
}

export function take_while(yielder, predicate) {
  let _pipe = yielder.continuation;
  let _pipe$1 = take_while_loop(_pipe, predicate);
  return new Yielder(_pipe$1);
}

function drop_while_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if (!$1) {
        return new Continue(e, next);
      } else {
        loop$continuation = next;
        loop$predicate = predicate;
      }
    }
  }
}

export function drop_while(yielder, predicate) {
  let _pipe = () => { return drop_while_loop(yielder.continuation, predicate); };
  return new Yielder(_pipe);
}

function scan_loop(continuation, f, accumulator) {
  return () => {
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let el = $[0];
      let next = $[1];
      let accumulated = f(accumulator, el);
      return new Continue(accumulated, scan_loop(next, f, accumulated));
    }
  };
}

export function scan(yielder, initial, f) {
  let _pipe = yielder.continuation;
  let _pipe$1 = scan_loop(_pipe, f, initial);
  return new Yielder(_pipe$1);
}

function zip_loop(left, right) {
  return () => {
    let $ = left();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let el_left = $[0];
      let next_left = $[1];
      let $1 = right();
      if ($1 instanceof Stop) {
        return new Stop();
      } else {
        let el_right = $1[0];
        let next_right = $1[1];
        return new Continue(
          [el_left, el_right],
          zip_loop(next_left, next_right),
        );
      }
    }
  };
}

export function zip(left, right) {
  let _pipe = zip_loop(left.continuation, right.continuation);
  return new Yielder(_pipe);
}

function next_chunk(
  loop$continuation,
  loop$f,
  loop$previous_key,
  loop$current_chunk
) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let previous_key = loop$previous_key;
    let current_chunk = loop$current_chunk;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new LastBy($list.reverse(current_chunk));
    } else {
      let e = $[0];
      let next = $[1];
      let key = f(e);
      let $1 = isEqual(key, previous_key);
      if ($1) {
        loop$continuation = next;
        loop$f = f;
        loop$previous_key = key;
        loop$current_chunk = listPrepend(e, current_chunk);
      } else {
        return new AnotherBy($list.reverse(current_chunk), key, e, next);
      }
    }
  }
}

function chunk_loop(continuation, f, previous_key, previous_element) {
  let $ = next_chunk(continuation, f, previous_key, toList([previous_element]));
  if ($ instanceof LastBy) {
    let chunk$1 = $[0];
    return new Continue(chunk$1, stop);
  } else {
    let chunk$1 = $[0];
    let key = $[1];
    let el = $[2];
    let next = $[3];
    return new Continue(chunk$1, () => { return chunk_loop(next, f, key, el); });
  }
}

export function chunk(yielder, f) {
  let _pipe = () => {
    let $ = yielder.continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      return chunk_loop(next, f, f(e), e);
    }
  };
  return new Yielder(_pipe);
}

function next_sized_chunk(loop$continuation, loop$left, loop$current_chunk) {
  while (true) {
    let continuation = loop$continuation;
    let left = loop$left;
    let current_chunk = loop$current_chunk;
    let $ = continuation();
    if ($ instanceof Stop) {
      if (current_chunk.hasLength(0)) {
        return new NoMore();
      } else {
        let remaining = current_chunk;
        return new Last($list.reverse(remaining));
      }
    } else {
      let e = $[0];
      let next = $[1];
      let chunk$1 = listPrepend(e, current_chunk);
      let $1 = left > 1;
      if (!$1) {
        return new Another($list.reverse(chunk$1), next);
      } else {
        loop$continuation = next;
        loop$left = left - 1;
        loop$current_chunk = chunk$1;
      }
    }
  }
}

function sized_chunk_loop(continuation, count) {
  return () => {
    let $ = next_sized_chunk(continuation, count, toList([]));
    if ($ instanceof NoMore) {
      return new Stop();
    } else if ($ instanceof Last) {
      let chunk$1 = $[0];
      return new Continue(chunk$1, stop);
    } else {
      let chunk$1 = $[0];
      let next_element = $[1];
      return new Continue(chunk$1, sized_chunk_loop(next_element, count));
    }
  };
}

export function sized_chunk(yielder, count) {
  let _pipe = yielder.continuation;
  let _pipe$1 = sized_chunk_loop(_pipe, count);
  return new Yielder(_pipe$1);
}

function intersperse_loop(continuation, separator) {
  let $ = continuation();
  if ($ instanceof Stop) {
    return new Stop();
  } else {
    let e = $[0];
    let next = $[1];
    let next_interspersed = () => { return intersperse_loop(next, separator); };
    return new Continue(
      separator,
      () => { return new Continue(e, next_interspersed); },
    );
  }
}

export function intersperse(yielder, elem) {
  let _pipe = () => {
    let $ = yielder.continuation();
    if ($ instanceof Stop) {
      return new Stop();
    } else {
      let e = $[0];
      let next = $[1];
      return new Continue(e, () => { return intersperse_loop(next, elem); });
    }
  };
  return new Yielder(_pipe);
}

function any_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return false;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        return true;
      } else {
        loop$continuation = next;
        loop$predicate = predicate;
      }
    }
  }
}

export function any(yielder, predicate) {
  let _pipe = yielder.continuation;
  return any_loop(_pipe, predicate);
}

function all_loop(loop$continuation, loop$predicate) {
  while (true) {
    let continuation = loop$continuation;
    let predicate = loop$predicate;
    let $ = continuation();
    if ($ instanceof Stop) {
      return true;
    } else {
      let e = $[0];
      let next = $[1];
      let $1 = predicate(e);
      if ($1) {
        loop$continuation = next;
        loop$predicate = predicate;
      } else {
        return false;
      }
    }
  }
}

export function all(yielder, predicate) {
  let _pipe = yielder.continuation;
  return all_loop(_pipe, predicate);
}

function update_group_with(el) {
  return (maybe_group) => {
    if (maybe_group instanceof Some) {
      let group$1 = maybe_group[0];
      return listPrepend(el, group$1);
    } else {
      return toList([el]);
    }
  };
}

function group_updater(f) {
  return (groups, elem) => {
    let _pipe = groups;
    return $dict.upsert(_pipe, f(elem), update_group_with(elem));
  };
}

export function group(yielder, key) {
  let _pipe = yielder;
  let _pipe$1 = fold(_pipe, $dict.new$(), group_updater(key));
  return $dict.map_values(
    _pipe$1,
    (_, group) => { return $list.reverse(group); },
  );
}

export function reduce(yielder, f) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else {
    let e = $[0];
    let next = $[1];
    let _pipe = fold_loop(next, f, e);
    return new Ok(_pipe);
  }
}

export function last(yielder) {
  let _pipe = yielder;
  return reduce(_pipe, (_, elem) => { return elem; });
}

export function empty() {
  return new Yielder(stop);
}

export function once(f) {
  let _pipe = () => { return new Continue(f(), stop); };
  return new Yielder(_pipe);
}

export function range(start, stop) {
  let $ = $int.compare(start, stop);
  if ($ instanceof $order.Eq) {
    return once(() => { return start; });
  } else if ($ instanceof $order.Gt) {
    return unfold(
      start,
      (current) => {
        let $1 = current < stop;
        if (!$1) {
          return new Next(current, current - 1);
        } else {
          return new Done();
        }
      },
    );
  } else {
    return unfold(
      start,
      (current) => {
        let $1 = current > stop;
        if (!$1) {
          return new Next(current, current + 1);
        } else {
          return new Done();
        }
      },
    );
  }
}

export function single(elem) {
  return once(() => { return elem; });
}

function interleave_loop(current, next) {
  let $ = current();
  if ($ instanceof Stop) {
    return next();
  } else {
    let e = $[0];
    let next_other = $[1];
    return new Continue(e, () => { return interleave_loop(next, next_other); });
  }
}

export function interleave(left, right) {
  let _pipe = () => {
    return interleave_loop(left.continuation, right.continuation);
  };
  return new Yielder(_pipe);
}

function fold_until_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Stop) {
      return accumulator;
    } else {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if ($1 instanceof $list.Continue) {
        let accumulator$1 = $1[0];
        loop$continuation = next;
        loop$f = f;
        loop$accumulator = accumulator$1;
      } else {
        let accumulator$1 = $1[0];
        return accumulator$1;
      }
    }
  }
}

export function fold_until(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return fold_until_loop(_pipe, f, initial);
}

function try_fold_loop(loop$continuation, loop$f, loop$accumulator) {
  while (true) {
    let continuation = loop$continuation;
    let f = loop$f;
    let accumulator = loop$accumulator;
    let $ = continuation();
    if ($ instanceof Stop) {
      return new Ok(accumulator);
    } else {
      let elem = $[0];
      let next = $[1];
      let $1 = f(accumulator, elem);
      if ($1.isOk()) {
        let result = $1[0];
        loop$continuation = next;
        loop$f = f;
        loop$accumulator = result;
      } else {
        let error = $1;
        return error;
      }
    }
  }
}

export function try_fold(yielder, initial, f) {
  let _pipe = yielder.continuation;
  return try_fold_loop(_pipe, f, initial);
}

export function first(yielder) {
  let $ = yielder.continuation();
  if ($ instanceof Stop) {
    return new Error(undefined);
  } else {
    let e = $[0];
    return new Ok(e);
  }
}

export function at(yielder, index) {
  let _pipe = yielder;
  let _pipe$1 = drop(_pipe, index);
  return first(_pipe$1);
}

function length_loop(loop$continuation, loop$length) {
  while (true) {
    let continuation = loop$continuation;
    let length = loop$length;
    let $ = continuation();
    if ($ instanceof Stop) {
      return length;
    } else {
      let next = $[1];
      loop$continuation = next;
      loop$length = length + 1;
    }
  }
}

export function length(yielder) {
  let _pipe = yielder.continuation;
  return length_loop(_pipe, 0);
}

export function each(yielder, f) {
  let _pipe = yielder;
  let _pipe$1 = map(_pipe, f);
  return run(_pipe$1);
}

export function yield$(element, next) {
  return new Yielder(
    () => {
      return new Continue(element, () => { return next().continuation(); });
    },
  );
}

export function prepend(yielder, element) {
  return yield$(element, () => { return yielder; });
}
