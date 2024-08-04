// build/dev/javascript/prelude.mjs
var CustomType = class {
  withFields(fields) {
    let properties = Object.keys(this).map(
      (label2) => label2 in fields ? fields[label2] : this[label2]
    );
    return new this.constructor(...properties);
  }
};
var List = class {
  static fromArray(array3, tail) {
    let t = tail || new Empty();
    for (let i = array3.length - 1; i >= 0; --i) {
      t = new NonEmpty(array3[i], t);
    }
    return t;
  }
  [Symbol.iterator]() {
    return new ListIterator(this);
  }
  toArray() {
    return [...this];
  }
  // @internal
  atLeastLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return true;
      desired--;
    }
    return desired <= 0;
  }
  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0)
        return false;
      desired--;
    }
    return desired === 0;
  }
  countLength() {
    let length3 = 0;
    for (let _ of this)
      length3++;
    return length3;
  }
};
function prepend(element2, tail) {
  return new NonEmpty(element2, tail);
}
function toList(elements, tail) {
  return List.fromArray(elements, tail);
}
var ListIterator = class {
  #current;
  constructor(current) {
    this.#current = current;
  }
  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
};
var Empty = class extends List {
};
var NonEmpty = class extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
};
var BitArray = class _BitArray {
  constructor(buffer) {
    if (!(buffer instanceof Uint8Array)) {
      throw "BitArray can only be constructed from a Uint8Array";
    }
    this.buffer = buffer;
  }
  // @internal
  get length() {
    return this.buffer.length;
  }
  // @internal
  byteAt(index2) {
    return this.buffer[index2];
  }
  // @internal
  floatFromSlice(start4, end, isBigEndian) {
    return byteArrayToFloat(this.buffer, start4, end, isBigEndian);
  }
  // @internal
  intFromSlice(start4, end, isBigEndian, isSigned) {
    return byteArrayToInt(this.buffer, start4, end, isBigEndian, isSigned);
  }
  // @internal
  binaryFromSlice(start4, end) {
    return new _BitArray(this.buffer.slice(start4, end));
  }
  // @internal
  sliceAfter(index2) {
    return new _BitArray(this.buffer.slice(index2));
  }
};
function byteArrayToInt(byteArray, start4, end, isBigEndian, isSigned) {
  let value3 = 0;
  if (isBigEndian) {
    for (let i = start4; i < end; i++) {
      value3 = value3 * 256 + byteArray[i];
    }
  } else {
    for (let i = end - 1; i >= start4; i--) {
      value3 = value3 * 256 + byteArray[i];
    }
  }
  if (isSigned) {
    const byteSize = end - start4;
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value3 >= highBit) {
      value3 -= highBit * 2;
    }
  }
  return value3;
}
function byteArrayToFloat(byteArray, start4, end, isBigEndian) {
  const view = new DataView(byteArray.buffer);
  const byteSize = end - start4;
  if (byteSize === 8) {
    return view.getFloat64(start4, !isBigEndian);
  } else if (byteSize === 4) {
    return view.getFloat32(start4, !isBigEndian);
  } else {
    const msg = `Sized floats must be 32-bit or 64-bit on JavaScript, got size of ${byteSize * 8} bits`;
    throw new globalThis.Error(msg);
  }
}
var Result = class _Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof _Result;
  }
};
var Ok = class extends Result {
  constructor(value3) {
    super();
    this[0] = value3;
  }
  // @internal
  isOk() {
    return true;
  }
};
var Error = class extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }
  // @internal
  isOk() {
    return false;
  }
};
function isEqual(x, y) {
  let values = [x, y];
  while (values.length) {
    let a2 = values.pop();
    let b = values.pop();
    if (a2 === b)
      continue;
    if (!isObject(a2) || !isObject(b))
      return false;
    let unequal = !structurallyCompatibleObjects(a2, b) || unequalDates(a2, b) || unequalBuffers(a2, b) || unequalArrays(a2, b) || unequalMaps(a2, b) || unequalSets(a2, b) || unequalRegExps(a2, b);
    if (unequal)
      return false;
    const proto = Object.getPrototypeOf(a2);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a2.equals(b))
          continue;
        else
          return false;
      } catch {
      }
    }
    let [keys2, get2] = getters(a2);
    for (let k of keys2(a2)) {
      values.push(get2(a2, k), get2(b, k));
    }
  }
  return true;
}
function getters(object3) {
  if (object3 instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object3 instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}
function unequalDates(a2, b) {
  return a2 instanceof Date && (a2 > b || a2 < b);
}
function unequalBuffers(a2, b) {
  return a2.buffer instanceof ArrayBuffer && a2.BYTES_PER_ELEMENT && !(a2.byteLength === b.byteLength && a2.every((n, i) => n === b[i]));
}
function unequalArrays(a2, b) {
  return Array.isArray(a2) && a2.length !== b.length;
}
function unequalMaps(a2, b) {
  return a2 instanceof Map && a2.size !== b.size;
}
function unequalSets(a2, b) {
  return a2 instanceof Set && (a2.size != b.size || [...a2].some((e) => !b.has(e)));
}
function unequalRegExps(a2, b) {
  return a2 instanceof RegExp && (a2.source !== b.source || a2.flags !== b.flags);
}
function isObject(a2) {
  return typeof a2 === "object" && a2 !== null;
}
function structurallyCompatibleObjects(a2, b) {
  if (typeof a2 !== "object" && typeof b !== "object" && (!a2 || !b))
    return false;
  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a2 instanceof c))
    return false;
  return a2.constructor === b.constructor;
}
function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.fn = fn;
  for (let k in extra)
    error[k] = extra[k];
  return error;
}

// build/dev/javascript/funtil/funtil.mjs
function never(loop$val) {
  while (true) {
    let val = loop$val;
    {
      let x = val[0];
      loop$val = x;
    }
  }
}

// build/dev/javascript/gleam_stdlib/gleam/bool.mjs
function guard(requirement, consequence, alternative) {
  if (requirement) {
    return consequence;
  } else {
    return alternative();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/option.mjs
var Some = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var None = class extends CustomType {
};
function to_result(option2, e) {
  if (option2 instanceof Some) {
    let a2 = option2[0];
    return new Ok(a2);
  } else {
    return new Error(e);
  }
}
function from_result(result) {
  if (result.isOk()) {
    let a2 = result[0];
    return new Some(a2);
  } else {
    return new None();
  }
}
function unwrap(option2, default$) {
  if (option2 instanceof Some) {
    let x = option2[0];
    return x;
  } else {
    return default$;
  }
}
function map(option2, fun) {
  if (option2 instanceof Some) {
    let x = option2[0];
    return new Some(fun(x));
  } else {
    return new None();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/int.mjs
function to_string2(x) {
  return to_string(x);
}

// build/dev/javascript/gleam_stdlib/gleam/pair.mjs
function first(pair) {
  let a2 = pair[0];
  return a2;
}
function map_second(pair, fun) {
  let a2 = pair[0];
  let b = pair[1];
  return [a2, fun(b)];
}

// build/dev/javascript/gleam_stdlib/gleam/list.mjs
function do_reverse(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest$1 = remaining.tail;
      loop$remaining = rest$1;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function reverse(xs) {
  return do_reverse(xs, toList([]));
}
function do_filter(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list.head;
      let xs = list.tail;
      let new_acc = (() => {
        let $ = fun(x);
        if ($) {
          return prepend(x, acc);
        } else {
          return acc;
        }
      })();
      loop$list = xs;
      loop$fun = fun;
      loop$acc = new_acc;
    }
  }
}
function filter(list, predicate) {
  return do_filter(list, predicate, toList([]));
}
function do_map(loop$list, loop$fun, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$fun = fun;
      loop$acc = prepend(fun(x), acc);
    }
  }
}
function map2(list, fun) {
  return do_map(list, fun, toList([]));
}
function do_index_map(loop$list, loop$fun, loop$index, loop$acc) {
  while (true) {
    let list = loop$list;
    let fun = loop$fun;
    let index2 = loop$index;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse(acc);
    } else {
      let x = list.head;
      let xs = list.tail;
      let acc$1 = prepend(fun(x, index2), acc);
      loop$list = xs;
      loop$fun = fun;
      loop$index = index2 + 1;
      loop$acc = acc$1;
    }
  }
}
function index_map(list, fun) {
  return do_index_map(list, fun, 0, toList([]));
}
function do_append(loop$first, loop$second) {
  while (true) {
    let first4 = loop$first;
    let second2 = loop$second;
    if (first4.hasLength(0)) {
      return second2;
    } else {
      let item = first4.head;
      let rest$1 = first4.tail;
      loop$first = rest$1;
      loop$second = prepend(item, second2);
    }
  }
}
function append(first4, second2) {
  return do_append(reverse(first4), second2);
}
function fold(loop$list, loop$initial, loop$fun) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    let fun = loop$fun;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest$1 = list.tail;
      loop$list = rest$1;
      loop$initial = fun(initial, x);
      loop$fun = fun;
    }
  }
}
function fold_right(list, initial, fun) {
  if (list.hasLength(0)) {
    return initial;
  } else {
    let x = list.head;
    let rest$1 = list.tail;
    return fun(fold_right(rest$1, initial, fun), x);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/result.mjs
function map3(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(fun(x));
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function map_error(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return new Ok(x);
  } else {
    let error = result[0];
    return new Error(fun(error));
  }
}
function try$(result, fun) {
  if (result.isOk()) {
    let x = result[0];
    return fun(x);
  } else {
    let e = result[0];
    return new Error(e);
  }
}
function then$(result, fun) {
  return try$(result, fun);
}
function unwrap2(result, default$) {
  if (result.isOk()) {
    let v = result[0];
    return v;
  } else {
    return default$;
  }
}

// build/dev/javascript/gleam_stdlib/gleam/string_builder.mjs
function from_strings(strings) {
  return concat(strings);
}
function from_string(string3) {
  return identity(string3);
}
function to_string3(builder) {
  return identity(builder);
}
function split2(iodata, pattern) {
  return split(iodata, pattern);
}

// build/dev/javascript/gleam_stdlib/gleam/string.mjs
function lowercase2(string3) {
  return lowercase(string3);
}
function join2(strings, separator) {
  return join(strings, separator);
}
function split3(x, substring) {
  if (substring === "") {
    return graphemes(x);
  } else {
    let _pipe = x;
    let _pipe$1 = from_string(_pipe);
    let _pipe$2 = split2(_pipe$1, substring);
    return map2(_pipe$2, to_string3);
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dynamic.mjs
var DecodeError = class extends CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
};
function from(a2) {
  return identity(a2);
}
function classify(data) {
  return classify_dynamic(data);
}
function int(data) {
  return decode_int(data);
}
function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify(data), toList([]))])
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}
function push_path(error, name) {
  let name$1 = from(name);
  let decoder = any(
    toList([string, (x) => {
      return map3(int(x), to_string2);
    }])
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$22 = $[0];
      return name$22;
    } else {
      let _pipe = toList(["<", classify(name$1), ">"]);
      let _pipe$1 = from_strings(_pipe);
      return to_string3(_pipe$1);
    }
  })();
  return error.withFields({ path: prepend(name$2, error.path) });
}
function map_errors(result, f) {
  return map_error(
    result,
    (_capture) => {
      return map2(_capture, f);
    }
  );
}
function string(data) {
  return decode_string(data);
}
function field(name, inner_type) {
  return (value3) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return try$(
      decode_field(value3, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => {
            return push_path(_capture, name);
          }
        );
      }
    );
  };
}

// build/dev/javascript/gleam_stdlib/dict.mjs
var referenceMap = /* @__PURE__ */ new WeakMap();
var tempDataView = new DataView(new ArrayBuffer(8));
var referenceUID = 0;
function hashByReference(o) {
  const known = referenceMap.get(o);
  if (known !== void 0) {
    return known;
  }
  const hash = referenceUID++;
  if (referenceUID === 2147483647) {
    referenceUID = 0;
  }
  referenceMap.set(o, hash);
  return hash;
}
function hashMerge(a2, b) {
  return a2 ^ b + 2654435769 + (a2 << 6) + (a2 >> 2) | 0;
}
function hashString(s) {
  let hash = 0;
  const len = s.length;
  for (let i = 0; i < len; i++) {
    hash = Math.imul(31, hash) + s.charCodeAt(i) | 0;
  }
  return hash;
}
function hashNumber(n) {
  tempDataView.setFloat64(0, n);
  const i = tempDataView.getInt32(0);
  const j = tempDataView.getInt32(4);
  return Math.imul(73244475, i >> 16 ^ i) ^ j;
}
function hashBigInt(n) {
  return hashString(n.toString());
}
function hashObject(o) {
  const proto = Object.getPrototypeOf(o);
  if (proto !== null && typeof proto.hashCode === "function") {
    try {
      const code = o.hashCode(o);
      if (typeof code === "number") {
        return code;
      }
    } catch {
    }
  }
  if (o instanceof Promise || o instanceof WeakSet || o instanceof WeakMap) {
    return hashByReference(o);
  }
  if (o instanceof Date) {
    return hashNumber(o.getTime());
  }
  let h = 0;
  if (o instanceof ArrayBuffer) {
    o = new Uint8Array(o);
  }
  if (Array.isArray(o) || o instanceof Uint8Array) {
    for (let i = 0; i < o.length; i++) {
      h = Math.imul(31, h) + getHash(o[i]) | 0;
    }
  } else if (o instanceof Set) {
    o.forEach((v) => {
      h = h + getHash(v) | 0;
    });
  } else if (o instanceof Map) {
    o.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
  } else {
    const keys2 = Object.keys(o);
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      const v = o[k];
      h = h + hashMerge(getHash(v), hashString(k)) | 0;
    }
  }
  return h;
}
function getHash(u) {
  if (u === null)
    return 1108378658;
  if (u === void 0)
    return 1108378659;
  if (u === true)
    return 1108378657;
  if (u === false)
    return 1108378656;
  switch (typeof u) {
    case "number":
      return hashNumber(u);
    case "string":
      return hashString(u);
    case "bigint":
      return hashBigInt(u);
    case "object":
      return hashObject(u);
    case "symbol":
      return hashByReference(u);
    case "function":
      return hashByReference(u);
    default:
      return 0;
  }
}
var SHIFT = 5;
var BUCKET_SIZE = Math.pow(2, SHIFT);
var MASK = BUCKET_SIZE - 1;
var MAX_INDEX_NODE = BUCKET_SIZE / 2;
var MIN_ARRAY_NODE = BUCKET_SIZE / 4;
var ENTRY = 0;
var ARRAY_NODE = 1;
var INDEX_NODE = 2;
var COLLISION_NODE = 3;
var EMPTY = {
  type: INDEX_NODE,
  bitmap: 0,
  array: []
};
function mask(hash, shift) {
  return hash >>> shift & MASK;
}
function bitpos(hash, shift) {
  return 1 << mask(hash, shift);
}
function bitcount(x) {
  x -= x >> 1 & 1431655765;
  x = (x & 858993459) + (x >> 2 & 858993459);
  x = x + (x >> 4) & 252645135;
  x += x >> 8;
  x += x >> 16;
  return x & 127;
}
function index(bitmap, bit) {
  return bitcount(bitmap & bit - 1);
}
function cloneAndSet(arr, at, val) {
  const len = arr.length;
  const out = new Array(len);
  for (let i = 0; i < len; ++i) {
    out[i] = arr[i];
  }
  out[at] = val;
  return out;
}
function spliceIn(arr, at, val) {
  const len = arr.length;
  const out = new Array(len + 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  out[g++] = val;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function spliceOut(arr, at) {
  const len = arr.length;
  const out = new Array(len - 1);
  let i = 0;
  let g = 0;
  while (i < at) {
    out[g++] = arr[i++];
  }
  ++i;
  while (i < len) {
    out[g++] = arr[i++];
  }
  return out;
}
function createNode(shift, key1, val1, key2hash, key2, val2) {
  const key1hash = getHash(key1);
  if (key1hash === key2hash) {
    return {
      type: COLLISION_NODE,
      hash: key1hash,
      array: [
        { type: ENTRY, k: key1, v: val1 },
        { type: ENTRY, k: key2, v: val2 }
      ]
    };
  }
  const addedLeaf = { val: false };
  return assoc(
    assocIndex(EMPTY, shift, key1hash, key1, val1, addedLeaf),
    shift,
    key2hash,
    key2,
    val2,
    addedLeaf
  );
}
function assoc(root2, shift, hash, key, val, addedLeaf) {
  switch (root2.type) {
    case ARRAY_NODE:
      return assocArray(root2, shift, hash, key, val, addedLeaf);
    case INDEX_NODE:
      return assocIndex(root2, shift, hash, key, val, addedLeaf);
    case COLLISION_NODE:
      return assocCollision(root2, shift, hash, key, val, addedLeaf);
  }
}
function assocArray(root2, shift, hash, key, val, addedLeaf) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size + 1,
      array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
    };
  }
  if (node.type === ENTRY) {
    if (isEqual(key, node.k)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: ARRAY_NODE,
        size: root2.size,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: ARRAY_NODE,
      size: root2.size,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, node.k, node.v, hash, key, val)
      )
    };
  }
  const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
  if (n === node) {
    return root2;
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function assocIndex(root2, shift, hash, key, val, addedLeaf) {
  const bit = bitpos(hash, shift);
  const idx = index(root2.bitmap, bit);
  if ((root2.bitmap & bit) !== 0) {
    const node = root2.array[idx];
    if (node.type !== ENTRY) {
      const n = assoc(node, shift + SHIFT, hash, key, val, addedLeaf);
      if (n === node) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    const nodeKey = node.k;
    if (isEqual(key, nodeKey)) {
      if (val === node.v) {
        return root2;
      }
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, {
          type: ENTRY,
          k: key,
          v: val
        })
      };
    }
    addedLeaf.val = true;
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap,
      array: cloneAndSet(
        root2.array,
        idx,
        createNode(shift + SHIFT, nodeKey, node.v, hash, key, val)
      )
    };
  } else {
    const n = root2.array.length;
    if (n >= MAX_INDEX_NODE) {
      const nodes = new Array(32);
      const jdx = mask(hash, shift);
      nodes[jdx] = assocIndex(EMPTY, shift + SHIFT, hash, key, val, addedLeaf);
      let j = 0;
      let bitmap = root2.bitmap;
      for (let i = 0; i < 32; i++) {
        if ((bitmap & 1) !== 0) {
          const node = root2.array[j++];
          nodes[i] = node;
        }
        bitmap = bitmap >>> 1;
      }
      return {
        type: ARRAY_NODE,
        size: n + 1,
        array: nodes
      };
    } else {
      const newArray = spliceIn(root2.array, idx, {
        type: ENTRY,
        k: key,
        v: val
      });
      addedLeaf.val = true;
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap | bit,
        array: newArray
      };
    }
  }
}
function assocCollision(root2, shift, hash, key, val, addedLeaf) {
  if (hash === root2.hash) {
    const idx = collisionIndexOf(root2, key);
    if (idx !== -1) {
      const entry = root2.array[idx];
      if (entry.v === val) {
        return root2;
      }
      return {
        type: COLLISION_NODE,
        hash,
        array: cloneAndSet(root2.array, idx, { type: ENTRY, k: key, v: val })
      };
    }
    const size = root2.array.length;
    addedLeaf.val = true;
    return {
      type: COLLISION_NODE,
      hash,
      array: cloneAndSet(root2.array, size, { type: ENTRY, k: key, v: val })
    };
  }
  return assoc(
    {
      type: INDEX_NODE,
      bitmap: bitpos(root2.hash, shift),
      array: [root2]
    },
    shift,
    hash,
    key,
    val,
    addedLeaf
  );
}
function collisionIndexOf(root2, key) {
  const size = root2.array.length;
  for (let i = 0; i < size; i++) {
    if (isEqual(key, root2.array[i].k)) {
      return i;
    }
  }
  return -1;
}
function find(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return findArray(root2, shift, hash, key);
    case INDEX_NODE:
      return findIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return findCollision(root2, key);
  }
}
function findArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    return void 0;
  }
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return void 0;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    return find(node, shift + SHIFT, hash, key);
  }
  if (isEqual(key, node.k)) {
    return node;
  }
  return void 0;
}
function findCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return void 0;
  }
  return root2.array[idx];
}
function without(root2, shift, hash, key) {
  switch (root2.type) {
    case ARRAY_NODE:
      return withoutArray(root2, shift, hash, key);
    case INDEX_NODE:
      return withoutIndex(root2, shift, hash, key);
    case COLLISION_NODE:
      return withoutCollision(root2, key);
  }
}
function withoutArray(root2, shift, hash, key) {
  const idx = mask(hash, shift);
  const node = root2.array[idx];
  if (node === void 0) {
    return root2;
  }
  let n = void 0;
  if (node.type === ENTRY) {
    if (!isEqual(node.k, key)) {
      return root2;
    }
  } else {
    n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
  }
  if (n === void 0) {
    if (root2.size <= MIN_ARRAY_NODE) {
      const arr = root2.array;
      const out = new Array(root2.size - 1);
      let i = 0;
      let j = 0;
      let bitmap = 0;
      while (i < idx) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      ++i;
      while (i < arr.length) {
        const nv = arr[i];
        if (nv !== void 0) {
          out[j] = nv;
          bitmap |= 1 << i;
          ++j;
        }
        ++i;
      }
      return {
        type: INDEX_NODE,
        bitmap,
        array: out
      };
    }
    return {
      type: ARRAY_NODE,
      size: root2.size - 1,
      array: cloneAndSet(root2.array, idx, n)
    };
  }
  return {
    type: ARRAY_NODE,
    size: root2.size,
    array: cloneAndSet(root2.array, idx, n)
  };
}
function withoutIndex(root2, shift, hash, key) {
  const bit = bitpos(hash, shift);
  if ((root2.bitmap & bit) === 0) {
    return root2;
  }
  const idx = index(root2.bitmap, bit);
  const node = root2.array[idx];
  if (node.type !== ENTRY) {
    const n = without(node, shift + SHIFT, hash, key);
    if (n === node) {
      return root2;
    }
    if (n !== void 0) {
      return {
        type: INDEX_NODE,
        bitmap: root2.bitmap,
        array: cloneAndSet(root2.array, idx, n)
      };
    }
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  if (isEqual(key, node.k)) {
    if (root2.bitmap === bit) {
      return void 0;
    }
    return {
      type: INDEX_NODE,
      bitmap: root2.bitmap ^ bit,
      array: spliceOut(root2.array, idx)
    };
  }
  return root2;
}
function withoutCollision(root2, key) {
  const idx = collisionIndexOf(root2, key);
  if (idx < 0) {
    return root2;
  }
  if (root2.array.length === 1) {
    return void 0;
  }
  return {
    type: COLLISION_NODE,
    hash: root2.hash,
    array: spliceOut(root2.array, idx)
  };
}
function forEach(root2, fn) {
  if (root2 === void 0) {
    return;
  }
  const items = root2.array;
  const size = items.length;
  for (let i = 0; i < size; i++) {
    const item = items[i];
    if (item === void 0) {
      continue;
    }
    if (item.type === ENTRY) {
      fn(item.v, item.k);
      continue;
    }
    forEach(item, fn);
  }
}
var Dict = class _Dict {
  /**
   * @template V
   * @param {Record<string,V>} o
   * @returns {Dict<string,V>}
   */
  static fromObject(o) {
    const keys2 = Object.keys(o);
    let m = _Dict.new();
    for (let i = 0; i < keys2.length; i++) {
      const k = keys2[i];
      m = m.set(k, o[k]);
    }
    return m;
  }
  /**
   * @template K,V
   * @param {Map<K,V>} o
   * @returns {Dict<K,V>}
   */
  static fromMap(o) {
    let m = _Dict.new();
    o.forEach((v, k) => {
      m = m.set(k, v);
    });
    return m;
  }
  static new() {
    return new _Dict(void 0, 0);
  }
  /**
   * @param {undefined | Node<K,V>} root
   * @param {number} size
   */
  constructor(root2, size) {
    this.root = root2;
    this.size = size;
  }
  /**
   * @template NotFound
   * @param {K} key
   * @param {NotFound} notFound
   * @returns {NotFound | V}
   */
  get(key, notFound) {
    if (this.root === void 0) {
      return notFound;
    }
    const found = find(this.root, 0, getHash(key), key);
    if (found === void 0) {
      return notFound;
    }
    return found.v;
  }
  /**
   * @param {K} key
   * @param {V} val
   * @returns {Dict<K,V>}
   */
  set(key, val) {
    const addedLeaf = { val: false };
    const root2 = this.root === void 0 ? EMPTY : this.root;
    const newRoot = assoc(root2, 0, getHash(key), key, val, addedLeaf);
    if (newRoot === this.root) {
      return this;
    }
    return new _Dict(newRoot, addedLeaf.val ? this.size + 1 : this.size);
  }
  /**
   * @param {K} key
   * @returns {Dict<K,V>}
   */
  delete(key) {
    if (this.root === void 0) {
      return this;
    }
    const newRoot = without(this.root, 0, getHash(key), key);
    if (newRoot === this.root) {
      return this;
    }
    if (newRoot === void 0) {
      return _Dict.new();
    }
    return new _Dict(newRoot, this.size - 1);
  }
  /**
   * @param {K} key
   * @returns {boolean}
   */
  has(key) {
    if (this.root === void 0) {
      return false;
    }
    return find(this.root, 0, getHash(key), key) !== void 0;
  }
  /**
   * @returns {[K,V][]}
   */
  entries() {
    if (this.root === void 0) {
      return [];
    }
    const result = [];
    this.forEach((v, k) => result.push([k, v]));
    return result;
  }
  /**
   *
   * @param {(val:V,key:K)=>void} fn
   */
  forEach(fn) {
    forEach(this.root, fn);
  }
  hashCode() {
    let h = 0;
    this.forEach((v, k) => {
      h = h + hashMerge(getHash(v), getHash(k)) | 0;
    });
    return h;
  }
  /**
   * @param {unknown} o
   * @returns {boolean}
   */
  equals(o) {
    if (!(o instanceof _Dict) || this.size !== o.size) {
      return false;
    }
    let equal = true;
    this.forEach((v, k) => {
      equal = equal && isEqual(o.get(k, !v), v);
    });
    return equal;
  }
};

// build/dev/javascript/gleam_stdlib/gleam_stdlib.mjs
var Nil = void 0;
var NOT_FOUND = {};
function identity(x) {
  return x;
}
function to_string(term) {
  return term.toString();
}
function graphemes(string3) {
  const iterator = graphemes_iterator(string3);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string3.match(/./gsu));
  }
}
function graphemes_iterator(string3) {
  if (Intl && Intl.Segmenter) {
    return new Intl.Segmenter().segment(string3)[Symbol.iterator]();
  }
}
function lowercase(string3) {
  return string3.toLowerCase();
}
function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}
function join(xs, separator) {
  const iterator = xs[Symbol.iterator]();
  let result = iterator.next().value || "";
  let current = iterator.next();
  while (!current.done) {
    result = result + separator + current.value;
    current = iterator.next();
  }
  return result;
}
function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}
function contains_string(haystack, needle) {
  return haystack.indexOf(needle) >= 0;
}
var unicode_whitespaces = [
  " ",
  // Space
  "	",
  // Horizontal tab
  "\n",
  // Line feed
  "\v",
  // Vertical tab
  "\f",
  // Form feed
  "\r",
  // Carriage return
  "\x85",
  // Next line
  "\u2028",
  // Line separator
  "\u2029"
  // Paragraph separator
].join();
var left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
var right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");
function new_map() {
  return Dict.new();
}
function map_to_list(map6) {
  return List.fromArray(map6.entries());
}
function map_get(map6, key) {
  const value3 = map6.get(key, NOT_FOUND);
  if (value3 === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value3);
}
function map_insert(key, value3, map6) {
  return map6.set(key, value3);
}
function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === void 0) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}
function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}
function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))])
  );
}
function decode_string(data) {
  return typeof data === "string" ? new Ok(data) : decoder_error("String", data);
}
function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}
function decode_field(value3, name) {
  const not_a_map_error = () => decoder_error("Dict", value3);
  if (value3 instanceof Dict || value3 instanceof WeakMap || value3 instanceof Map) {
    const entry = map_get(value3, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value3 === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value3) == Object.prototype) {
    return try_get_field(value3, name, () => new Ok(new None()));
  } else {
    return try_get_field(value3, name, not_a_map_error);
  }
}
function try_get_field(value3, field3, or_else) {
  try {
    return field3 in value3 ? new Ok(new Some(value3[field3])) : or_else();
  } catch {
    return or_else();
  }
}

// build/dev/javascript/gleam_stdlib/gleam/dict.mjs
function new$() {
  return new_map();
}
function get(from3, get2) {
  return map_get(from3, get2);
}
function insert(dict, key, value3) {
  return map_insert(key, value3, dict);
}
function fold_list_of_pair(loop$list, loop$initial) {
  while (true) {
    let list = loop$list;
    let initial = loop$initial;
    if (list.hasLength(0)) {
      return initial;
    } else {
      let x = list.head;
      let rest = list.tail;
      loop$list = rest;
      loop$initial = insert(initial, x[0], x[1]);
    }
  }
}
function from_list(list) {
  return fold_list_of_pair(list, new$());
}
function reverse_and_concat(loop$remaining, loop$accumulator) {
  while (true) {
    let remaining = loop$remaining;
    let accumulator = loop$accumulator;
    if (remaining.hasLength(0)) {
      return accumulator;
    } else {
      let item = remaining.head;
      let rest = remaining.tail;
      loop$remaining = rest;
      loop$accumulator = prepend(item, accumulator);
    }
  }
}
function do_keys_acc(loop$list, loop$acc) {
  while (true) {
    let list = loop$list;
    let acc = loop$acc;
    if (list.hasLength(0)) {
      return reverse_and_concat(acc, toList([]));
    } else {
      let x = list.head;
      let xs = list.tail;
      loop$list = xs;
      loop$acc = prepend(x[0], acc);
    }
  }
}
function do_keys(dict) {
  let list_of_pairs = map_to_list(dict);
  return do_keys_acc(list_of_pairs, toList([]));
}
function keys(dict) {
  return do_keys(dict);
}
function upsert(dict, key, fun) {
  let _pipe = dict;
  let _pipe$1 = get(_pipe, key);
  let _pipe$2 = from_result(_pipe$1);
  let _pipe$3 = fun(_pipe$2);
  return ((_capture) => {
    return insert(dict, key, _capture);
  })(_pipe$3);
}

// build/dev/javascript/gleam_stdlib/gleam/function.mjs
function apply1(fun, arg1) {
  return fun(arg1);
}

// build/dev/javascript/gleam_stdlib/gleam/uri.mjs
var Uri = class extends CustomType {
  constructor(scheme, userinfo, host, port, path, query, fragment2) {
    super();
    this.scheme = scheme;
    this.userinfo = userinfo;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
    this.fragment = fragment2;
  }
};
function do_remove_dot_segments(loop$input, loop$accumulator) {
  while (true) {
    let input3 = loop$input;
    let accumulator = loop$accumulator;
    if (input3.hasLength(0)) {
      return reverse(accumulator);
    } else {
      let segment = input3.head;
      let rest = input3.tail;
      let accumulator$1 = (() => {
        if (segment === "") {
          let accumulator$12 = accumulator;
          return accumulator$12;
        } else if (segment === ".") {
          let accumulator$12 = accumulator;
          return accumulator$12;
        } else if (segment === ".." && accumulator.hasLength(0)) {
          return toList([]);
        } else if (segment === ".." && accumulator.atLeastLength(1)) {
          let accumulator$12 = accumulator.tail;
          return accumulator$12;
        } else {
          let segment$1 = segment;
          let accumulator$12 = accumulator;
          return prepend(segment$1, accumulator$12);
        }
      })();
      loop$input = rest;
      loop$accumulator = accumulator$1;
    }
  }
}
function remove_dot_segments(input3) {
  return do_remove_dot_segments(input3, toList([]));
}
function path_segments(path) {
  return remove_dot_segments(split3(path, "/"));
}

// build/dev/javascript/justin/justin.mjs
function add2(words, word) {
  if (word === "") {
    return words;
  } else {
    return prepend(word, words);
  }
}
function is_upper(g) {
  return lowercase2(g) !== g;
}
function split4(loop$in, loop$up, loop$word, loop$words) {
  while (true) {
    let in$ = loop$in;
    let up = loop$up;
    let word = loop$word;
    let words = loop$words;
    if (in$.hasLength(0) && word === "") {
      return reverse(words);
    } else if (in$.hasLength(0)) {
      return reverse(add2(words, word));
    } else if (in$.atLeastLength(1) && in$.head === "\n") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "	") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "!") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "?") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "#") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === ".") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "-") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === "_") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else if (in$.atLeastLength(1) && in$.head === " ") {
      let in$1 = in$.tail;
      loop$in = in$1;
      loop$up = false;
      loop$word = "";
      loop$words = add2(words, word);
    } else {
      let g = in$.head;
      let in$1 = in$.tail;
      let $ = is_upper(g);
      if (!$) {
        loop$in = in$1;
        loop$up = false;
        loop$word = word + g;
        loop$words = words;
      } else if ($ && up) {
        loop$in = in$1;
        loop$up = up;
        loop$word = word + g;
        loop$words = words;
      } else {
        loop$in = in$1;
        loop$up = true;
        loop$word = g;
        loop$words = add2(words, word);
      }
    }
  }
}
function split_words(text3) {
  let _pipe = text3;
  let _pipe$1 = graphemes(_pipe);
  return split4(_pipe$1, false, "", toList([]));
}
function kebab_case(text3) {
  let _pipe = text3;
  let _pipe$1 = split_words(_pipe);
  let _pipe$2 = join2(_pipe$1, "-");
  return lowercase2(_pipe$2);
}

// build/dev/javascript/lustre/lustre/effect.mjs
var Effect = class extends CustomType {
  constructor(all) {
    super();
    this.all = all;
  }
};
function from2(effect2) {
  return new Effect(toList([(dispatch, _) => {
    return effect2(dispatch);
  }]));
}
function none() {
  return new Effect(toList([]));
}
function batch(effects) {
  return new Effect(
    fold(
      effects,
      toList([]),
      (b, _use1) => {
        let a2 = _use1.all;
        return append(b, a2);
      }
    )
  );
}

// build/dev/javascript/lustre/lustre/internals/vdom.mjs
var Text = class extends CustomType {
  constructor(content) {
    super();
    this.content = content;
  }
};
var Element = class extends CustomType {
  constructor(key, namespace, tag, attrs, children, self_closing, void$) {
    super();
    this.key = key;
    this.namespace = namespace;
    this.tag = tag;
    this.attrs = attrs;
    this.children = children;
    this.self_closing = self_closing;
    this.void = void$;
  }
};
var Map2 = class extends CustomType {
  constructor(subtree) {
    super();
    this.subtree = subtree;
  }
};
var Fragment = class extends CustomType {
  constructor(elements, key) {
    super();
    this.elements = elements;
    this.key = key;
  }
};
var Attribute = class extends CustomType {
  constructor(x0, x1, as_property) {
    super();
    this[0] = x0;
    this[1] = x1;
    this.as_property = as_property;
  }
};
var Event = class extends CustomType {
  constructor(x0, x1) {
    super();
    this[0] = x0;
    this[1] = x1;
  }
};

// build/dev/javascript/lustre/lustre/attribute.mjs
function attribute(name, value3) {
  return new Attribute(name, from(value3), false);
}
function on(name, handler) {
  return new Event("on" + name, handler);
}
function map4(attr, f) {
  if (attr instanceof Attribute) {
    let name$1 = attr[0];
    let value$1 = attr[1];
    let as_property = attr.as_property;
    return new Attribute(name$1, value$1, as_property);
  } else {
    let on$1 = attr[0];
    let handler = attr[1];
    return new Event(on$1, (e) => {
      return map3(handler(e), f);
    });
  }
}
function style(properties) {
  return attribute(
    "style",
    fold(
      properties,
      "",
      (styles, _use1) => {
        let name$1 = _use1[0];
        let value$1 = _use1[1];
        return styles + name$1 + ":" + value$1 + ";";
      }
    )
  );
}
function class$(name) {
  return attribute("class", name);
}
function value(val) {
  return attribute("value", val);
}
function placeholder(text3) {
  return attribute("placeholder", text3);
}
function href(uri) {
  return attribute("href", uri);
}

// build/dev/javascript/lustre/lustre/element.mjs
function element(tag, attrs, children) {
  if (tag === "area") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "base") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "br") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "col") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "embed") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "hr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "img") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "input") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "link") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "meta") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "param") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "source") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "track") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else if (tag === "wbr") {
    return new Element("", "", tag, attrs, toList([]), false, true);
  } else {
    return new Element("", "", tag, attrs, children, false, false);
  }
}
function do_keyed(el2, key) {
  if (el2 instanceof Element) {
    let namespace = el2.namespace;
    let tag = el2.tag;
    let attrs = el2.attrs;
    let children = el2.children;
    let self_closing = el2.self_closing;
    let void$ = el2.void;
    return new Element(
      key,
      namespace,
      tag,
      attrs,
      children,
      self_closing,
      void$
    );
  } else if (el2 instanceof Map2) {
    let subtree = el2.subtree;
    return new Map2(() => {
      return do_keyed(subtree(), key);
    });
  } else if (el2 instanceof Fragment) {
    let elements = el2.elements;
    let _pipe = elements;
    let _pipe$1 = index_map(
      _pipe,
      (element2, idx) => {
        if (element2 instanceof Element) {
          let el_key = element2.key;
          let new_key = (() => {
            if (el_key === "") {
              return key + "-" + to_string2(idx);
            } else {
              return key + "-" + el_key;
            }
          })();
          return do_keyed(element2, new_key);
        } else {
          return do_keyed(element2, key);
        }
      }
    );
    return new Fragment(_pipe$1, key);
  } else {
    return el2;
  }
}
function keyed(el2, children) {
  return el2(
    map2(
      children,
      (_use0) => {
        let key = _use0[0];
        let child = _use0[1];
        return do_keyed(child, key);
      }
    )
  );
}
function text(content) {
  return new Text(content);
}
function none2() {
  return new Text("");
}
function flatten_fragment_elements(elements) {
  return fold_right(
    elements,
    toList([]),
    (new_elements, element2) => {
      if (element2 instanceof Fragment) {
        let fr_elements = element2.elements;
        return append(fr_elements, new_elements);
      } else {
        let el2 = element2;
        return prepend(el2, new_elements);
      }
    }
  );
}
function fragment(elements) {
  let _pipe = flatten_fragment_elements(elements);
  return new Fragment(_pipe, "");
}
function map5(element2, f) {
  if (element2 instanceof Text) {
    let content = element2.content;
    return new Text(content);
  } else if (element2 instanceof Map2) {
    let subtree = element2.subtree;
    return new Map2(() => {
      return map5(subtree(), f);
    });
  } else if (element2 instanceof Element) {
    let key = element2.key;
    let namespace = element2.namespace;
    let tag = element2.tag;
    let attrs = element2.attrs;
    let children = element2.children;
    let self_closing = element2.self_closing;
    let void$ = element2.void;
    return new Map2(
      () => {
        return new Element(
          key,
          namespace,
          tag,
          map2(
            attrs,
            (_capture) => {
              return map4(_capture, f);
            }
          ),
          map2(children, (_capture) => {
            return map5(_capture, f);
          }),
          self_closing,
          void$
        );
      }
    );
  } else {
    let elements = element2.elements;
    let key = element2.key;
    return new Map2(
      () => {
        return new Fragment(
          map2(elements, (_capture) => {
            return map5(_capture, f);
          }),
          key
        );
      }
    );
  }
}

// build/dev/javascript/lustre/lustre/internals/runtime.mjs
var Debug = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Dispatch = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};
var Shutdown = class extends CustomType {
};
var ForceModel = class extends CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
};

// build/dev/javascript/lustre/vdom.ffi.mjs
function morph(prev, next, dispatch, isComponent = false) {
  let out;
  let stack = [{ prev, next, parent: prev.parentNode }];
  while (stack.length) {
    let { prev: prev2, next: next2, parent } = stack.pop();
    if (next2.subtree !== void 0)
      next2 = next2.subtree();
    if (next2.content !== void 0) {
      if (!prev2) {
        const created = document.createTextNode(next2.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev2.nodeType === Node.TEXT_NODE) {
        if (prev2.textContent !== next2.content)
          prev2.textContent = next2.content;
        out ??= prev2;
      } else {
        const created = document.createTextNode(next2.content);
        parent.replaceChild(created, prev2);
        out ??= created;
      }
    } else if (next2.tag !== void 0) {
      const created = createElementNode({
        prev: prev2,
        next: next2,
        dispatch,
        stack,
        isComponent
      });
      if (!prev2) {
        parent.appendChild(created);
      } else if (prev2 !== created) {
        parent.replaceChild(created, prev2);
      }
      out ??= created;
    } else if (next2.elements !== void 0) {
      iterateElement(next2, (fragmentElement) => {
        stack.unshift({ prev: prev2, next: fragmentElement, parent });
        prev2 = prev2?.nextSibling;
      });
    } else if (next2.subtree !== void 0) {
      stack.push({ prev: prev2, next: next2, parent });
    }
  }
  return out;
}
function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const canMorph = prev && prev.nodeType === Node.ELEMENT_NODE && prev.localName === next.tag && prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el2 = canMorph ? prev : namespace ? document.createElementNS(namespace, next.tag) : document.createElement(next.tag);
  let handlersForEl;
  if (!registeredHandlers.has(el2)) {
    const emptyHandlers = /* @__PURE__ */ new Map();
    registeredHandlers.set(el2, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el2);
  }
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph ? new Set(Array.from(prev.attributes, (a2) => a2.name)) : null;
  let className = null;
  let style2 = null;
  let innerHTML = null;
  for (const attr of next.attrs) {
    const name = attr[0];
    const value3 = attr[1];
    if (attr.as_property) {
      if (el2[name] !== value3)
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value3);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      if (canMorph)
        prevHandlers.delete(eventName);
    } else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);
      if (!handlersForEl.has(eventName)) {
        el2.addEventListener(eventName, lustreGenericEventHandler);
      }
      handlersForEl.set(eventName, callback);
      el2.setAttribute(name, value3);
    } else if (name === "class") {
      className = className === null ? value3 : className + " " + value3;
    } else if (name === "style") {
      style2 = style2 === null ? value3 : style2 + value3;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value3;
    } else {
      if (el2.getAttribute(name) !== value3)
        el2.setAttribute(name, value3);
      if (name === "value" || name === "selected")
        el2[name] = value3;
      if (canMorph)
        prevAttributes.delete(name);
    }
  }
  if (className !== null) {
    el2.setAttribute("class", className);
    if (canMorph)
      prevAttributes.delete("class");
  }
  if (style2 !== null) {
    el2.setAttribute("style", style2);
    if (canMorph)
      prevAttributes.delete("style");
  }
  if (canMorph) {
    for (const attr of prevAttributes) {
      el2.removeAttribute(attr);
    }
    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el2.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }
  if (next.key !== void 0 && next.key !== "") {
    el2.setAttribute("data-lustre-key", next.key);
  } else if (innerHTML !== null) {
    el2.innerHTML = innerHTML;
    return el2;
  }
  let prevChild = el2.firstChild;
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;
  if (canMorph && firstChild !== void 0 && // Explicit checks are more verbose but truthy checks force a bunch of comparisons
  // we don't care about: it's never gonna be a number etc.
  firstChild.key !== void 0 && firstChild.key !== "") {
    seenKeys = /* @__PURE__ */ new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    iterateElement(child, (currElement) => {
      if (currElement.key !== void 0 && seenKeys !== null) {
        prevChild = diffKeyedChild(
          prevChild,
          currElement,
          el2,
          stack,
          incomingKeyedChildren,
          keyedChildren,
          seenKeys
        );
      } else {
        stack.unshift({ prev: prevChild, next: currElement, parent: el2 });
        prevChild = prevChild?.nextSibling;
      }
    });
  }
  while (prevChild) {
    const next2 = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = next2;
  }
  return el2;
}
var registeredHandlers = /* @__PURE__ */ new WeakMap();
function lustreGenericEventHandler(event2) {
  const target = event2.currentTarget;
  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  const handlersForEventTarget = registeredHandlers.get(target);
  if (!handlersForEventTarget.has(event2.type)) {
    target.removeEventListener(event2.type, lustreGenericEventHandler);
    return;
  }
  handlersForEventTarget.get(event2.type)(event2);
}
function lustreServerEventHandler(event2) {
  const el2 = event2.currentTarget;
  const tag = el2.getAttribute(`data-lustre-on-${event2.type}`);
  const data = JSON.parse(el2.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el2.getAttribute("data-lustre-include") || "[]");
  switch (event2.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }
  return {
    tag,
    data: include.reduce(
      (data2, property) => {
        const path = property.split(".");
        for (let i = 0, o = data2, e = event2; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }
        return data2;
      },
      { data }
    )
  };
}
function getKeyedChildren(el2) {
  const keyedChildren = /* @__PURE__ */ new Map();
  if (el2) {
    for (const child of el2.children) {
      iterateElement(child, (currElement) => {
        const key = currElement?.key || currElement?.getAttribute?.("data-lustre-key");
        if (key)
          keyedChildren.set(key, currElement);
      });
    }
  }
  return keyedChildren;
}
function diffKeyedChild(prevChild, child, el2, stack, incomingKeyedChildren, keyedChildren, seenKeys) {
  while (prevChild && !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))) {
    const nextChild = prevChild.nextSibling;
    el2.removeChild(prevChild);
    prevChild = nextChild;
  }
  if (keyedChildren.size === 0) {
    iterateElement(child, (currChild) => {
      stack.unshift({ prev: prevChild, next: currChild, parent: el2 });
      prevChild = prevChild?.nextSibling;
    });
    return prevChild;
  }
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  seenKeys.add(child.key);
  const keyedChild = keyedChildren.get(child.key);
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild && prevChild !== null) {
    const placeholder2 = document.createTextNode("");
    el2.insertBefore(placeholder2, prevChild);
    stack.unshift({ prev: placeholder2, next: child, parent: el2 });
    return prevChild;
  }
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el2 });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }
  el2.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el2 });
  return prevChild;
}
function iterateElement(element2, processElement) {
  if (element2.elements !== void 0) {
    for (const currElement of element2.elements) {
      processElement(currElement);
    }
  } else if (element2.subtree !== void 0) {
    iterateElement(element2.subtree(), processElement);
  } else {
    processElement(element2);
  }
}

// build/dev/javascript/lustre/client-runtime.ffi.mjs
var LustreClientApplication2 = class _LustreClientApplication {
  #root = null;
  #queue = [];
  #effects = [];
  #didUpdate = false;
  #isComponent = false;
  #model = null;
  #update = null;
  #view = null;
  static start(flags, selector, init3, update, view) {
    if (!is_browser())
      return new Error(new NotABrowser());
    const root2 = selector instanceof HTMLElement ? selector : document.querySelector(selector);
    if (!root2)
      return new Error(new ElementNotFound(selector));
    const app = new _LustreClientApplication(init3(flags), update, view, root2);
    return new Ok((msg) => app.send(msg));
  }
  constructor([model, effects], update, view, root2 = document.body, isComponent = false) {
    this.#model = model;
    this.#update = update;
    this.#view = view;
    this.#root = root2;
    this.#effects = effects.all.toArray();
    this.#didUpdate = true;
    this.#isComponent = isComponent;
    window.requestAnimationFrame(() => this.#tick());
  }
  send(action) {
    switch (true) {
      case action instanceof Dispatch: {
        this.#queue.push(action[0]);
        this.#tick();
        return;
      }
      case action instanceof Shutdown: {
        this.#shutdown();
        return;
      }
      case action instanceof Debug: {
        this.#debug(action[0]);
        return;
      }
      default:
        return;
    }
  }
  emit(event2, data) {
    this.#root.dispatchEvent(
      new CustomEvent(event2, {
        bubbles: true,
        detail: data,
        composed: true
      })
    );
  }
  #tick() {
    this.#flush_queue();
    if (this.#didUpdate) {
      const vdom = this.#view(this.#model);
      const dispatch = (handler) => (e) => {
        const result = handler(e);
        if (result instanceof Ok) {
          this.send(new Dispatch(result[0]));
        }
      };
      this.#didUpdate = false;
      this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
    }
  }
  #flush_queue(iterations = 0) {
    while (this.#queue.length) {
      const [next, effects] = this.#update(this.#model, this.#queue.shift());
      this.#didUpdate ||= this.#model !== next;
      this.#model = next;
      this.#effects = this.#effects.concat(effects.all.toArray());
    }
    while (this.#effects.length) {
      this.#effects.shift()(
        (msg) => this.send(new Dispatch(msg)),
        (event2, data) => this.emit(event2, data)
      );
    }
    if (this.#queue.length) {
      if (iterations < 5) {
        this.#flush_queue(++iterations);
      } else {
        window.requestAnimationFrame(() => this.#tick());
      }
    }
  }
  #debug(action) {
    switch (true) {
      case action instanceof ForceModel: {
        const vdom = this.#view(action[0]);
        const dispatch = (handler) => (e) => {
          const result = handler(e);
          if (result instanceof Ok) {
            this.send(new Dispatch(result[0]));
          }
        };
        this.#queue = [];
        this.#effects = [];
        this.#didUpdate = false;
        this.#root = morph(this.#root, vdom, dispatch, this.#isComponent);
      }
    }
  }
  #shutdown() {
    this.#root.remove();
    this.#root = null;
    this.#model = null;
    this.#queue = [];
    this.#effects = [];
    this.#didUpdate = false;
    this.#update = () => {
    };
    this.#view = () => {
    };
  }
};
var start = (app, selector, flags) => LustreClientApplication2.start(
  flags,
  selector,
  app.init,
  app.update,
  app.view
);
var is_browser = () => globalThis.window && window.document;

// build/dev/javascript/lustre/client-component.ffi.mjs
function register({ init: init3, update, view, on_attribute_change }, name) {
  if (!is_browser())
    return new Error(new NotABrowser());
  if (!name.includes("-"))
    return new Error(new BadComponentName(name));
  if (window.customElements.get(name)) {
    return new Error(new ComponentAlreadyRegistered(name));
  }
  const Component2 = makeComponent(init3, update, view, on_attribute_change);
  window.customElements.define(name, Component2);
  for (const el2 of document.querySelectorAll(name)) {
    const replaced = new Component2();
    for (const attr of el2.attributes) {
      replaced.setAttribute(attr.name, attr.value);
    }
    el2.replaceWith(replaced);
  }
  return new Ok(void 0);
}
function makeComponent(init3, update, view, on_attribute_change) {
  return class LustreClientComponent extends HTMLElement {
    #root = document.createElement("div");
    #application = null;
    #shadow = null;
    #styles = [];
    static get observedAttributes() {
      return on_attribute_change[0]?.entries().map(([name, _]) => name) ?? [];
    }
    constructor() {
      super();
      this.#shadow = this.attachShadow({ mode: "closed" });
      on_attribute_change[0]?.forEach((decoder, name) => {
        Object.defineProperty(this, name, {
          get() {
            return this[`_${name}`] || this.getAttribute(name);
          },
          set(value3) {
            const prev = this[name];
            const decoded = decoder(value3);
            const shouldDispatch = decoded instanceof Ok && (!this.#application || !isEqual(prev, value3));
            if (shouldDispatch) {
              this.#application ? this.#application.send(new Dispatch(decoded[0])) : window.requestAnimationFrame(
                () => this.#application.send(new Dispatch(decoded[0]))
              );
            }
            this[`_${name}`] = value3;
          }
        });
      });
    }
    connectedCallback() {
      this.#application = new LustreClientApplication2(
        init3(),
        update,
        view,
        this.#root,
        true
      );
      this.#adoptStyleSheets().finally(() => {
        this.#shadow.append(this.#root);
      });
    }
    attributeChangedCallback(key, _, next) {
      this[key] = next;
    }
    disconnectedCallback() {
      this.#application.send(new Shutdown());
    }
    #adoptStyleSheets() {
      while (this.#styles.length)
        this.#styles.shift().remove();
      const pending = [];
      this.#shadow.adoptedStyleSheets = this.getRootNode().adoptedStyleSheets;
      for (const sheet of document.styleSheets) {
        try {
          this.#shadow.adoptedStyleSheets.push(sheet);
        } catch {
          const node = sheet.ownerNode.cloneNode();
          this.#shadow.appendChild(node);
          pending.push(
            new Promise((resolve, reject) => {
              node.onload = resolve;
              node.onerror = reject;
            })
          );
        }
      }
      return Promise.allSettled(pending);
    }
    adoptStyleSheet(sheet) {
      this.#shadow.adoptedStyleSheets.push(sheet);
    }
    get adoptedStyleSheets() {
      return this.#shadow.adoptedStyleSheets;
    }
  };
}

// build/dev/javascript/lustre/lustre.mjs
var App = class extends CustomType {
  constructor(init3, update, view, on_attribute_change) {
    super();
    this.init = init3;
    this.update = update;
    this.view = view;
    this.on_attribute_change = on_attribute_change;
  }
};
var BadComponentName = class extends CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
};
var ComponentAlreadyRegistered = class extends CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
};
var ElementNotFound = class extends CustomType {
  constructor(selector) {
    super();
    this.selector = selector;
  }
};
var NotABrowser = class extends CustomType {
};
function application(init3, update, view) {
  return new App(init3, update, view, new None());
}
function start3(app, selector, flags) {
  return guard(
    !is_browser(),
    new Error(new NotABrowser()),
    () => {
      return start(app, selector, flags);
    }
  );
}

// build/dev/javascript/lustre/lustre/element/html.mjs
function text2(content) {
  return text(content);
}
function aside(attrs, children) {
  return element("aside", attrs, children);
}
function h1(attrs, children) {
  return element("h1", attrs, children);
}
function h2(attrs, children) {
  return element("h2", attrs, children);
}
function main(attrs, children) {
  return element("main", attrs, children);
}
function nav(attrs, children) {
  return element("nav", attrs, children);
}
function div(attrs, children) {
  return element("div", attrs, children);
}
function li(attrs, children) {
  return element("li", attrs, children);
}
function p(attrs, children) {
  return element("p", attrs, children);
}
function ul(attrs, children) {
  return element("ul", attrs, children);
}
function a(attrs, children) {
  return element("a", attrs, children);
}
function span(attrs, children) {
  return element("span", attrs, children);
}
function input(attrs) {
  return element("input", attrs, toList([]));
}
function label(attrs, children) {
  return element("label", attrs, children);
}

// build/dev/javascript/lustre/lustre/event.mjs
function on2(name, handler) {
  return on(name, handler);
}
function value2(event2) {
  let _pipe = event2;
  return field("target", field("value", string))(
    _pipe
  );
}
function on_input(msg) {
  return on2(
    "input",
    (event2) => {
      let _pipe = value2(event2);
      return map3(_pipe, msg);
    }
  );
}

// build/dev/javascript/modem/modem.ffi.mjs
var defaults = {
  handle_external_links: false,
  handle_internal_links: true
};
var initial_location = window?.location?.href;
var do_initial_uri = () => {
  if (!initial_location) {
    return new Error(void 0);
  } else {
    return new Ok(uri_from_url(new URL(initial_location)));
  }
};
var do_init = (dispatch, options = defaults) => {
  document.addEventListener("click", (event2) => {
    const a2 = find_anchor(event2.target);
    if (!a2)
      return;
    try {
      const url = new URL(a2.href);
      const uri = uri_from_url(url);
      const is_external = url.host !== window.location.host;
      if (!options.handle_external_links && is_external)
        return;
      if (!options.handle_internal_links && !is_external)
        return;
      event2.preventDefault();
      if (!is_external) {
        window.history.pushState({}, "", a2.href);
        window.requestAnimationFrame(() => {
          if (url.hash) {
            document.getElementById(url.hash.slice(1))?.scrollIntoView();
          }
        });
      }
      return dispatch(uri);
    } catch {
      return;
    }
  });
  window.addEventListener("popstate", (e) => {
    e.preventDefault();
    const url = new URL(window.location.href);
    const uri = uri_from_url(url);
    window.requestAnimationFrame(() => {
      if (url.hash) {
        document.getElementById(url.hash.slice(1))?.scrollIntoView();
      }
    });
    dispatch(uri);
  });
  window.addEventListener("modem-push", ({ detail }) => {
    dispatch(detail);
  });
  window.addEventListener("modem-replace", ({ detail }) => {
    dispatch(detail);
  });
};
var find_anchor = (el2) => {
  if (!el2 || el2.tagName === "BODY") {
    return null;
  } else if (el2.tagName === "A") {
    return el2;
  } else {
    return find_anchor(el2.parentElement);
  }
};
var uri_from_url = (url) => {
  return new Uri(
    /* scheme   */
    url.protocol ? new Some(url.protocol.slice(0, -1)) : new None(),
    /* userinfo */
    new None(),
    /* host     */
    url.hostname ? new Some(url.hostname) : new None(),
    /* port     */
    url.port ? new Some(Number(url.port)) : new None(),
    /* path     */
    url.pathname,
    /* query    */
    url.search ? new Some(url.search.slice(1)) : new None(),
    /* fragment */
    url.hash ? new Some(url.hash.slice(1)) : new None()
  );
};

// build/dev/javascript/modem/modem.mjs
function init2(handler) {
  return from2(
    (dispatch) => {
      return guard(
        !is_browser(),
        void 0,
        () => {
          return do_init(
            (uri) => {
              let _pipe = uri;
              let _pipe$1 = handler(_pipe);
              return dispatch(_pipe$1);
            }
          );
        }
      );
    }
  );
}

// build/dev/javascript/fable/fable/middleware.mjs
var Middleware = class extends CustomType {
  constructor(run) {
    super();
    this.run = run;
  }
};
var Component = class extends CustomType {
  constructor(init3, effects, controls, render2) {
    super();
    this.init = init3;
    this.effects = effects;
    this.controls = controls;
    this.render = render2;
  }
};
var State = class extends CustomType {
  constructor(get2, set, update) {
    super();
    this.get = get2;
    this.set = set;
    this.update = update;
  }
};
var Translation = class extends CustomType {
  constructor(get2, map6) {
    super();
    this.get = get2;
    this.map = map6;
  }
};
function effect(eff, next) {
  return new Middleware(
    (rest_to_model) => {
      let $ = next().run(rest_to_model);
      let state$1 = $.init;
      let effects = $.effects;
      let controls = $.controls;
      let render2 = $.render;
      return new Component(state$1, prepend(eff, effects), controls, render2);
    }
  );
}
function return$(render2) {
  return new Middleware(
    (_) => {
      return new Component(void 0, toList([]), toList([]), render2);
    }
  );
}
function control(render_control, next) {
  return new Middleware(
    (rest_to_model) => {
      let $ = next().run(rest_to_model);
      let state$1 = $.init;
      let effects = $.effects;
      let controls = $.controls;
      let render2 = $.render;
      return new Component(
        state$1,
        effects,
        prepend(render_control, controls),
        render2
      );
    }
  );
}
function compose_translations(a2, b) {
  let get2 = (outer) => {
    let _pipe = outer;
    let _pipe$1 = b.get(_pipe);
    return a2.get(_pipe$1);
  };
  let map6 = (f) => {
    return b.map(a2.map(f));
  };
  return new Translation(get2, map6);
}
function identity3() {
  return new Translation(
    (x) => {
      return x;
    },
    (f) => {
      return (x) => {
        return f(x);
      };
    }
  );
}
function compose(middleware) {
  let $ = middleware.run(identity3());
  let model = $.init;
  let effects = $.effects;
  let controls = $.controls;
  let render2 = $.render;
  let init3 = (_) => {
    return [
      model,
      batch(
        map2(
          effects,
          (_capture) => {
            return apply1(_capture, model);
          }
        )
      )
    ];
  };
  let update = (model2, f) => {
    let next = f(model2);
    let effects$1 = batch(
      map2(
        effects,
        (_capture) => {
          return apply1(_capture, next);
        }
      )
    );
    return [next, effects$1];
  };
  let view = (model2) => {
    return render2(
      model2,
      map2(
        controls,
        (_capture) => {
          return apply1(_capture, model2);
        }
      )
    );
  };
  return application(init3, update, view);
}
function tuple_second_translation() {
  let get2 = (t) => {
    return t[1];
  };
  let map6 = (f) => {
    return (t) => {
      return [t[0], f(t[1])];
    };
  };
  return new Translation(get2, map6);
}
function tuple_first_state() {
  let get2 = first;
  let set = (a2) => {
    return (t) => {
      return [a2, t[1]];
    };
  };
  let update = (f) => {
    return (t) => {
      return [f(t[0]), t[1]];
    };
  };
  return new State(get2, set, update);
}
function compose_with_state(a2, b) {
  let get2 = (c) => {
    let _pipe = c;
    let _pipe$1 = a2.get(_pipe);
    return b.get(_pipe$1);
  };
  let set = (new$4) => {
    return (c) => {
      let _pipe = c;
      return a2.map(b.set(new$4))(_pipe);
    };
  };
  let update = (f) => {
    return (c) => {
      let _pipe = c;
      return a2.map(b.update(f))(_pipe);
    };
  };
  return new State(get2, set, update);
}
function state(init3, next) {
  return new Middleware(
    (rest_to_outer) => {
      let inner_to_outer = compose_translations(
        tuple_second_translation(),
        rest_to_outer
      );
      let state$1 = compose_with_state(rest_to_outer, tuple_first_state());
      let $ = next(state$1).run(inner_to_outer);
      let rest = $.init;
      let eff = $.effects;
      let controls = $.controls;
      let render2 = $.render;
      return new Component([init3, rest], eff, controls, render2);
    }
  );
}

// build/dev/javascript/fable/fable.mjs
var Book = class extends CustomType {
  constructor(chapters) {
    super();
    this.chapters = chapters;
  }
};
var Story = class extends CustomType {
  constructor(title, name, controls) {
    super();
    this.title = title;
    this.name = name;
    this.controls = controls;
  }
};
function book() {
  return new Book(new$());
}
function story(title, component) {
  let name = (() => {
    let _pipe = title;
    let _pipe$1 = lowercase2(_pipe);
    return kebab_case(_pipe$1);
  })();
  return new Story(title, name, component());
}
function input2(label2, next) {
  return state(
    "",
    (value3) => {
      return control(
        (model) => {
          return label(
            toList([class$("grid grid-cols-3 gap-2")]),
            toList([
              span(
                toList([class$("col-span-1 text-right")]),
                toList([text2(label2)])
              ),
              input(
                toList([
                  class$("col-span-2 px-2 py-1"),
                  value(value3.get(model)),
                  on_input(value3.set)
                ])
              )
            ])
          );
        },
        () => {
          return next(value3);
        }
      );
    }
  );
}
function render(component) {
  return return$(
    (model, controls) => {
      return div(
        toList([class$("w-full h-full flex flex-col")]),
        toList([
          div(
            toList([
              class$("flex-1 flex justify-center items-center")
            ]),
            toList([component(model)])
          ),
          div(
            toList([
              class$(
                " max-h-[300px] overflow-y-scroll flex flex-col gap-2 bg-gray-50\n            p-4 border-t border-gray-200\n          "
              )
            ]),
            controls
          )
        ])
      );
    }
  );
}
function to_component(story2) {
  let app = compose(story2.controls);
  return map3(
    register(app, story2.name + "-story"),
    (_) => {
      return (attributes, children) => {
        return element(story2.name + "-story", attributes, children);
      };
    }
  );
}
function add_story(book2, chapter, story2) {
  let $ = to_component(story2);
  if (!$.isOk()) {
    return book2;
  } else {
    let component = $[0];
    return new Book(
      upsert(
        book2.chapters,
        chapter,
        (stories) => {
          let _pipe = stories;
          let _pipe$1 = map(
            _pipe,
            (_capture) => {
              return insert(
                _capture,
                story2.name,
                component(toList([]), toList([]))
              );
            }
          );
          return unwrap(
            _pipe$1,
            from_list(
              toList([[story2.name, component(toList([]), toList([]))]])
            )
          );
        }
      )
    );
  }
}
function to_app(book2) {
  let sidebar = (() => {
    let _pipe = book2.chapters;
    let _pipe$1 = map_to_list(_pipe);
    return map2(
      _pipe$1,
      (_capture) => {
        return map_second(_capture, keys);
      }
    );
  })();
  let $ = do_initial_uri();
  if (!$.isOk()) {
    throw makeError(
      "assignment_no_match",
      "fable",
      216,
      "to_app",
      "Assignment pattern did not match",
      { value: $ }
    );
  }
  let uri = $[0];
  return compose(
    state(
      "",
      (search) => {
        return state(
          uri.path,
          (route) => {
            return state(
              false,
              (loaded) => {
                return effect(
                  (model) => {
                    return guard(
                      loaded.get(model),
                      none(),
                      () => {
                        let set_loaded = from2(
                          (_capture) => {
                            return apply1(_capture, loaded.set(true));
                          }
                        );
                        let init_router = init2(
                          (uri2) => {
                            return route.set(uri2.path);
                          }
                        );
                        return batch(toList([set_loaded, init_router]));
                      }
                    );
                  },
                  () => {
                    return return$(
                      (state2, _) => {
                        let search_input = input(
                          toList([
                            value(search.get(state2)),
                            placeholder("Search..."),
                            on_input(search.set)
                          ])
                        );
                        let stories = map2(
                          sidebar,
                          (section) => {
                            return fragment(
                              toList([
                                h2(
                                  toList([]),
                                  toList([text2(section[0])])
                                ),
                                keyed(
                                  (_capture) => {
                                    return ul(toList([]), _capture);
                                  },
                                  (() => {
                                    let _pipe = section[1];
                                    let _pipe$1 = filter(
                                      _pipe,
                                      (_capture) => {
                                        return contains_string(
                                          _capture,
                                          search.get(state2)
                                        );
                                      }
                                    );
                                    return map2(
                                      _pipe$1,
                                      (story2) => {
                                        let href2 = "/" + section[0] + "/" + story2;
                                        let colour = (() => {
                                          let $1 = route.get(state2) === href2;
                                          if ($1) {
                                            return "text-blue-500";
                                          } else {
                                            return "text-gray-800";
                                          }
                                        })();
                                        let html = li(
                                          toList([
                                            class$("ml-2 " + colour)
                                          ]),
                                          toList([
                                            a(
                                              toList([href(href2)]),
                                              toList([text2(story2)])
                                            )
                                          ])
                                        );
                                        return [story2, html];
                                      }
                                    );
                                  })()
                                )
                              ])
                            );
                          }
                        );
                        return div(
                          toList([
                            class$(
                              "flex w-screen h-screen items-stretch"
                            )
                          ]),
                          toList([
                            aside(
                              toList([
                                class$(
                                  "w-[300px] h-full bg-gray-100 overflow-y-scroll p-4 flex flex-col gap-4"
                                )
                              ]),
                              toList([
                                h1(
                                  toList([class$("text-2xl")]),
                                  toList([text2("Fable")])
                                ),
                                search_input,
                                nav(toList([]), stories)
                              ])
                            ),
                            main(
                              toList([class$("flex-1")]),
                              toList([
                                (() => {
                                  let $1 = (() => {
                                    let _pipe = state2;
                                    let _pipe$1 = route.get(_pipe);
                                    return path_segments(_pipe$1);
                                  })();
                                  if ($1.hasLength(2)) {
                                    let chapter = $1.head;
                                    let story$1 = $1.tail.head;
                                    let _pipe = book2.chapters;
                                    let _pipe$1 = get(_pipe, chapter);
                                    let _pipe$2 = then$(
                                      _pipe$1,
                                      (_capture) => {
                                        return get(_capture, story$1);
                                      }
                                    );
                                    let _pipe$3 = unwrap2(
                                      _pipe$2,
                                      none2()
                                    );
                                    return map5(_pipe$3, never);
                                  } else {
                                    return none2();
                                  }
                                })()
                              ])
                            )
                          ])
                        );
                      }
                    );
                  }
                );
              }
            );
          }
        );
      }
    )
  );
}

// build/dev/javascript/demo/ui/field.mjs
function field2(handle_input, label2, value3, error) {
  let wrapper_styles = toList([
    ["display", "flex"],
    ["flex-direction", "column"]
  ]);
  let label_styles = toList([["font-size", "0.8em"]]);
  let input_styles = toList([
    ["padding", "0.5em 1em"],
    ["border", "1px solid"],
    [
      "border-color",
      (() => {
        if (error === "") {
          return "#ccc";
        } else {
          return "red";
        }
      })()
    ],
    ["border-radius", "0.5em"]
  ]);
  let error_styles = toList([
    ["align-self", "flex-end"],
    ["font-size", "0.8em"],
    ["color", "red"]
  ]);
  return label(
    toList([style(wrapper_styles)]),
    toList([
      p(
        toList([style(label_styles)]),
        toList([text2(label2)])
      ),
      input(
        toList([
          style(input_styles),
          value(value3),
          on_input(handle_input)
        ])
      ),
      p(
        toList([style(error_styles)]),
        toList([text2(error)])
      )
    ])
  );
}
function field_story() {
  return story(
    "field",
    () => {
      return input2(
        "Label value",
        (label2) => {
          return input2(
            "Input value",
            (value3) => {
              return input2(
                "Error message",
                (error) => {
                  return render(
                    (state2) => {
                      return field2(
                        value3.set,
                        label2.get(state2),
                        value3.get(state2),
                        error.get(state2)
                      );
                    }
                  );
                }
              );
            }
          );
        }
      );
    }
  );
}
function field_with_label_story() {
  return story(
    "field with label",
    () => {
      return input2(
        "Input value",
        (value3) => {
          return render(
            (state2) => {
              return field2(value3.set, "username", value3.get(state2), "");
            }
          );
        }
      );
    }
  );
}
function field_with_message_story() {
  return story(
    "field with message",
    () => {
      return input2(
        "Input value",
        (value3) => {
          return render(
            (state2) => {
              return field2(
                value3.set,
                "username",
                value3.get(state2),
                "username already exists."
              );
            }
          );
        }
      );
    }
  );
}

// build/dev/javascript/demo/demo.mjs
function main2() {
  let _pipe = book();
  let _pipe$1 = add_story(_pipe, "components", field_story());
  let _pipe$2 = add_story(
    _pipe$1,
    "components",
    field_with_label_story()
  );
  let _pipe$3 = add_story(
    _pipe$2,
    "components",
    field_with_message_story()
  );
  let _pipe$4 = to_app(_pipe$3);
  return start3(_pipe$4, "#app", void 0);
}

// build/.lustre/entry.mjs
main2();
