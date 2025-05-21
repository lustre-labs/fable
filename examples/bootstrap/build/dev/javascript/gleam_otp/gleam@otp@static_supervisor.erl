-module(gleam@otp@static_supervisor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, restart_tolerance/3, auto_shutdown/2, add/2, worker_child/2, supervisor_child/2, significant/2, timeout/2, restart/2, start_link/1, init/1]).
-export_type([strategy/0, auto_shutdown/0, builder/0, restart/0, child_type/0, child_builder/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Bindings to Erlang/OTP's `supervisor` module.\n"
    "\n"
    " For further detail see the Erlang documentation:\n"
    " <https://www.erlang.org/doc/apps/stdlib/supervisor.html>.\n"
    "\n"
    " # Example\n"
    "\n"
    " ```gleam\n"
    " import gleam/erlang/process.{type Pid}\n"
    " import gleam/otp/static_supervisor as sup\n"
    " \n"
    " pub fn start_supervisor() {\n"
    "   sup.new(sup.OneForOne)\n"
    "   |> sup.add(sup.worker_child(\"db\", start_database_connection))\n"
    "   |> sup.add(sup.worker_child(\"workers\", start_workers))\n"
    "   |> sup.add(sup.worker_child(\"web\", start_http_server))\n"
    "   |> sup.start_link\n"
    " }\n"
    " ```\n"
).

-type strategy() :: one_for_one | one_for_all | rest_for_one.

-type auto_shutdown() :: never | any_significant | all_significant.

-opaque builder() :: {builder,
        strategy(),
        integer(),
        integer(),
        auto_shutdown(),
        list(child_builder())}.

-type restart() :: permanent | transient | temporary.

-type child_type() :: {worker, integer()} | supervisor.

-opaque child_builder() :: {child_builder,
        binary(),
        fun(() -> {ok, gleam@erlang@process:pid_()} |
            {error, gleam@dynamic:dynamic_()}),
        restart(),
        boolean(),
        child_type()}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 74).
-spec new(strategy()) -> builder().
new(Strategy) ->
    {builder, Strategy, 2, 5, never, []}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 94).
?DOC(
    " To prevent a supervisor from getting into an infinite loop of child\n"
    " process terminations and restarts, a maximum restart intensity is\n"
    " defined using two integer values specified with keys intensity and\n"
    " period in the above map. Assuming the values MaxR for intensity and MaxT\n"
    " for period, then, if more than MaxR restarts occur within MaxT seconds,\n"
    " the supervisor terminates all child processes and then itself. The\n"
    " termination reason for the supervisor itself in that case will be\n"
    " shutdown. \n"
    "\n"
    " Intensity defaults to 1 and period defaults to 5.\n"
).
-spec restart_tolerance(builder(), integer(), integer()) -> builder().
restart_tolerance(Builder, Intensity, Period) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        Intensity,
        Period,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 104).
?DOC(
    " A supervisor can be configured to automatically shut itself down with\n"
    " exit reason shutdown when significant children terminate.\n"
).
-spec auto_shutdown(builder(), auto_shutdown()) -> builder().
auto_shutdown(Builder, Value) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Value,
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 184).
?DOC(" Add a child to the supervisor.\n").
-spec add(builder(), child_builder()) -> builder().
add(Builder, Child) ->
    _record = Builder,
    {builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        erlang:element(5, _record),
        [Child | erlang:element(6, Builder)]}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 197).
?DOC(
    " A regular child that is not also a supervisor.\n"
    "\n"
    " id is used to identify the child specification internally by the\n"
    " supervisor.\n"
    " Notice that this identifier on occations has been called \"name\". As far\n"
    " as possible, the terms \"identifier\" or \"id\" are now used but to keep\n"
    " backward compatibility, some occurences of \"name\" can still be found, for\n"
    " example in error messages.\n"
).
-spec worker_child(
    binary(),
    fun(() -> {ok, gleam@erlang@process:pid_()} | {error, any()})
) -> child_builder().
worker_child(Id, Starter) ->
    {child_builder, Id, fun() -> _pipe = Starter(),
            gleam@result:map_error(_pipe, fun gleam_stdlib:identity/1) end, permanent, false, {worker,
            5000}}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 219).
?DOC(
    " A special child that is a supervisor itself.\n"
    "\n"
    " id is used to identify the child specification internally by the\n"
    " supervisor.\n"
    " Notice that this identifier on occations has been called \"name\". As far\n"
    " as possible, the terms \"identifier\" or \"id\" are now used but to keep\n"
    " backward compatibility, some occurences of \"name\" can still be found, for\n"
    " example in error messages.\n"
).
-spec supervisor_child(
    binary(),
    fun(() -> {ok, gleam@erlang@process:pid_()} | {error, any()})
) -> child_builder().
supervisor_child(Id, Starter) ->
    {child_builder, Id, fun() -> _pipe = Starter(),
            gleam@result:map_error(_pipe, fun gleam_stdlib:identity/1) end, permanent, false, supervisor}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 241).
?DOC(
    " This defines if a child is considered significant for automatic\n"
    " self-shutdown of the supervisor.\n"
    "\n"
    " You most likely do not want to consider any children significant.\n"
    "\n"
    " This will be ignored if the supervisor auto shutdown is set to `Never`,\n"
    " which is the default.\n"
    "\n"
    " The default value for significance is `False`.\n"
).
-spec significant(child_builder(), boolean()) -> child_builder().
significant(Child, Significant) ->
    _record = Child,
    {child_builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        erlang:element(4, _record),
        Significant,
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 252).
?DOC(
    " This defines the amount of milliseconds a child has to shut down before\n"
    " being brutal killed by the supervisor.\n"
    "\n"
    " If not set the default for a child is 5000ms.\n"
    "\n"
    " This will be ignored if the child is a supervisor itself.\n"
).
-spec timeout(child_builder(), integer()) -> child_builder().
timeout(Child, Ms) ->
    case erlang:element(6, Child) of
        {worker, _} ->
            _record = Child,
            {child_builder,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                {worker, Ms}};

        _ ->
            Child
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 263).
?DOC(
    " When the child is to be restarted. See the `Restart` documentation for\n"
    " more.\n"
    "\n"
    " The default value for restart is `Permanent`.\n"
).
-spec restart(child_builder(), restart()) -> child_builder().
restart(Child, Restart) ->
    _record = Child,
    {child_builder,
        erlang:element(2, _record),
        erlang:element(3, _record),
        Restart,
        erlang:element(5, _record),
        erlang:element(6, _record)}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 294).
-spec property(
    gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()),
    binary(),
    any()
) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
property(Dict, Key, Value) ->
    gleam@dict:insert(
        Dict,
        erlang:binary_to_atom(Key),
        gleam_stdlib:identity(Value)
    ).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 267).
-spec convert_child(child_builder()) -> gleam@dict:dict(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic_()).
convert_child(Child) ->
    Mfa = {erlang:binary_to_atom(<<"erlang"/utf8>>),
        erlang:binary_to_atom(<<"apply"/utf8>>),
        [gleam_stdlib:identity(erlang:element(3, Child)),
            gleam_stdlib:identity([])]},
    {Type_, Shutdown} = case erlang:element(6, Child) of
        supervisor ->
            {erlang:binary_to_atom(<<"supervisor"/utf8>>),
                gleam_stdlib:identity(
                    erlang:binary_to_atom(<<"infinity"/utf8>>)
                )};

        {worker, Timeout} ->
            {erlang:binary_to_atom(<<"worker"/utf8>>),
                gleam_stdlib:identity(Timeout)}
    end,
    _pipe = maps:new(),
    _pipe@1 = property(_pipe, <<"id"/utf8>>, erlang:element(2, Child)),
    _pipe@2 = property(_pipe@1, <<"start"/utf8>>, Mfa),
    _pipe@3 = property(_pipe@2, <<"restart"/utf8>>, erlang:element(4, Child)),
    _pipe@4 = property(
        _pipe@3,
        <<"significant"/utf8>>,
        erlang:element(5, Child)
    ),
    _pipe@5 = property(_pipe@4, <<"type"/utf8>>, Type_),
    property(_pipe@5, <<"shutdown"/utf8>>, Shutdown).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 165).
-spec start_link(builder()) -> {ok, gleam@erlang@process:pid_()} |
    {error, gleam@dynamic:dynamic_()}.
start_link(Builder) ->
    Flags = begin
        _pipe = maps:new(),
        _pipe@1 = property(
            _pipe,
            <<"strategy"/utf8>>,
            erlang:element(2, Builder)
        ),
        _pipe@2 = property(
            _pipe@1,
            <<"intensity"/utf8>>,
            erlang:element(3, Builder)
        ),
        _pipe@3 = property(
            _pipe@2,
            <<"period"/utf8>>,
            erlang:element(4, Builder)
        ),
        property(_pipe@3, <<"auto_shutdown"/utf8>>, erlang:element(5, Builder))
    end,
    Children = begin
        _pipe@4 = erlang:element(6, Builder),
        _pipe@5 = lists:reverse(_pipe@4),
        gleam@list:map(_pipe@5, fun convert_child/1)
    end,
    gleam_otp_external:static_supervisor_start_link({Flags, Children}).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/static_supervisor.gleam", 304).
?DOC(false).
-spec init(gleam@dynamic:dynamic_()) -> {ok, gleam@dynamic:dynamic_()} |
    {error, any()}.
init(Start_data) ->
    {ok, Start_data}.
