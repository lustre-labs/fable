-module(gleam@fetch@form_data).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([form_data/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " `FormData` are common structures on the web to send both string data, and\n"
    " blob. They're the default standard when using a `<form>` on a web page,\n"
    " and they're still a simple way to send files from a frontend to a backend.\n"
    "\n"
    " To simplify management of form data, JavaScript exposes a structure called\n"
    " `FormData` that handles all the complicated details for you. JavaScript\n"
    " `FormData` are compatible with every standards functions, like `fetch` or\n"
    " `xmlHttpRequest`.\n"
    "\n"
    " To maximise compatibility between JavaScript and Gleam, `gleam_fetch`\n"
    " exposes bindings to JavaScript\n"
    " [`FormData`](https://developer.mozilla.org/docs/Web/API/FormData).\n"
).

-type form_data() :: any().


