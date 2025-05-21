-module(gleam@fetch).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([fetch_error/0, fetch_body/0, fetch_request/0, fetch_response/0]).

-type fetch_error() :: {network_error, binary()} |
    unable_to_read_body |
    invalid_json_body.

-type fetch_body() :: any().

-type fetch_request() :: any().

-type fetch_response() :: any().


