import {
  Ok,
  Error,
  toList,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";

export class Some extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class None extends $CustomType {}

function reverse_and_prepend(loop$prefix, loop$suffix) {
  while (true) {
    let prefix = loop$prefix;
    let suffix = loop$suffix;
    if (prefix.hasLength(0)) {
      return suffix;
    } else {
      let first = prefix.head;
      let rest = prefix.tail;
      loop$prefix = rest;
      loop$suffix = listPrepend(first, suffix);
    }
  }
}

function reverse(list) {
  return reverse_and_prepend(list, toList([]));
}

function all_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return new Some(reverse(acc));
    } else if (list.atLeastLength(1) && list.head instanceof None) {
      return new None();
    } else {
      let first = list.head[0];
      let rest = list.tail;
      loop$list = rest;
      loop$acc = listPrepend(first, acc);
    }
  }
}

export function all(list) {
  return all_loop(list, toList([]));
}

export function is_some(option) {
  return !isEqual(option, new None());
}

export function is_none(option) {
  return isEqual(option, new None());
}

export function to_result(option, e) {
  if (option instanceof Some) {
    let a = option[0];
    return new Ok(a);
  } else {
    return new Error(e);
  }
}

export function from_result(result) {
  if (result.isOk()) {
    let a = result[0];
    return new Some(a);
  } else {
    return new None();
  }
}

export function unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$;
  }
}

export function lazy_unwrap(option, default$) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return default$();
  }
}

export function map(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}

export function flatten(option) {
  if (option instanceof Some) {
    let x = option[0];
    return x;
  } else {
    return new None();
  }
}

export function then$(option, fun) {
  if (option instanceof Some) {
    let x = option[0];
    return fun(x);
  } else {
    return new None();
  }
}

export function or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second;
  }
}

export function lazy_or(first, second) {
  if (first instanceof Some) {
    return first;
  } else {
    return second();
  }
}

function values_loop(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else if (list.atLeastLength(1) && list.head instanceof None) {
      let rest = list.tail;
      loop$list = rest;
      loop$acc = acc;
    } else {
      let first = list.head[0];
      let rest = list.tail;
      loop$list = rest;
      loop$acc = listPrepend(first, acc);
    }
  }
}

export function values(options) {
  return values_loop(options, toList([]));
}
