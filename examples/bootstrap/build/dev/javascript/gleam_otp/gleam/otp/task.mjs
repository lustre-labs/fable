import * as $process from "../../../gleam_erlang/gleam/erlang/process.mjs";
import * as $dict from "../../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../../gleam_stdlib/gleam/dynamic.mjs";
import * as $function from "../../../gleam_stdlib/gleam/function.mjs";
import * as $list from "../../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../../gleam_stdlib/gleam/option.mjs";
import { toList, prepend as listPrepend, CustomType as $CustomType } from "../../gleam.mjs";

class Task extends $CustomType {
  constructor(owner, pid, subject) {
    super();
    this.owner = owner;
    this.pid = pid;
    this.subject = subject;
  }
}

export class Timeout extends $CustomType {}

export class Exit extends $CustomType {
  constructor(reason) {
    super();
    this.reason = reason;
  }
}

class M2FromSubject1 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M2FromSubject2 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M2Timeout extends $CustomType {}

class M3FromSubject1 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M3FromSubject2 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M3FromSubject3 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M3Timeout extends $CustomType {}

class M4FromSubject1 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M4FromSubject2 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M4FromSubject3 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M4FromSubject4 extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

class M4Timeout extends $CustomType {}

class Message extends $CustomType {
  constructor(from, value) {
    super();
    this.from = from;
    this.value = value;
  }
}

class MessageTimeout extends $CustomType {}

export function pid(task) {
  return task.pid;
}

function dict_to_list_loop(loop$dict, loop$default, loop$index, loop$list) {
  while (true) {
    let dict = loop$dict;
    let default$ = loop$default;
    let index = loop$index;
    let list = loop$list;
    let $ = index < 0;
    if ($) {
      return list;
    } else {
      let _block;
      let $1 = $dict.get(dict, index);
      if (!$1.isOk()) {
        _block = default$;
      } else {
        let value = $1[0];
        _block = value;
      }
      let value = _block;
      loop$dict = dict;
      loop$default = default$;
      loop$index = index - 1;
      loop$list = listPrepend(value, list);
    }
  }
}

function dict_to_list(dict, sized, default$) {
  return dict_to_list_loop(dict, default$, sized - 1, toList([]));
}
