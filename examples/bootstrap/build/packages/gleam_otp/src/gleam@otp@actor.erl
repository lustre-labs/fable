-module(gleam@otp@actor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([continue/1, with_selector/2, to_erlang_start_result/1, start_spec/1, start/2, send/2, call/3]).
-export_type([message/1, next/2, init_result/2, self/2, spec/2, start_error/0, start_init_message/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " This module provides the _Actor_ abstraction, one of the most common\n"
    " building blocks of Gleam OTP programs.\n"
    " \n"
    " An Actor is a process like any other BEAM process and can be used to hold\n"
    " state, execute code, and communicate with other processes by sending and\n"
    " receiving messages. The advantage of using the actor abstraction over a bare\n"
    " process is that it provides a single interface for commonly needed\n"
    " functionality, including support for the [tracing and debugging\n"
    " features in OTP][erlang-sys].\n"
    "\n"
    " Gleam's Actor is similar to Erlang's `gen_server` and Elixir's `GenServer`\n"
    " but differs in that it offers a fully typed interface. This different API is\n"
    " why Gleam uses the name Actor rather than some variation of generic-server.\n"
    "\n"
    " [erlang-sys]: https://www.erlang.org/doc/man/sys.html\n"
    "\n"
    " ## Example\n"
    "\n"
    " An Actor can be used to create a client-server interaction between an Actor\n"
    " (the server) and other processes (the clients). In this example we have an\n"
    " Actor that works as a stack, allowing clients to push and pop elements.\n"
    "\n"
    " ```gleam\n"
    " pub fn main() {\n"
    "   // Start the actor with initial state of an empty list, and the\n"
    "   // `handle_message` callback function (defined below).\n"
    "   // We assert that it starts successfully.\n"
    "   // \n"
    "   // In real-world Gleam OTP programs we would likely write wrapper functions\n"
    "   // called `start`, `push` `pop`, `shutdown` to start and interact with the\n"
    "   // Actor. We are not doing that here for the sake of showing how the Actor \n"
    "   // API works.\n"
    "   let assert Ok(my_actor) = actor.start([], handle_message)\n"
    " \n"
    "   // We can send a message to the actor to push elements onto the stack.\n"
    "   process.send(my_actor, Push(\"Joe\"))\n"
    "   process.send(my_actor, Push(\"Mike\"))\n"
    "   process.send(my_actor, Push(\"Robert\"))\n"
    " \n"
    "   // The `Push` message expects no response, these messages are sent purely for\n"
    "   // the side effect of mutating the state held by the actor.\n"
    "   //\n"
    "   // We can also send the `Pop` message to take a value off of the actor's\n"
    "   // stack. This message expects a response, so we use `process.call` to send a\n"
    "   // message and wait until a reply is received.\n"
    "   //\n"
    "   // In this instance we are giving the actor 10 milliseconds to reply, if the\n"
    "   // `call` function doesn't get a reply within this time it will panic and\n"
    "   // crash the client process.\n"
    "   let assert Ok(\"Robert\") = process.call(my_actor, Pop, 10)\n"
    "   let assert Ok(\"Mike\") = process.call(my_actor, Pop, 10)\n"
    "   let assert Ok(\"Joe\") = process.call(my_actor, Pop, 10)\n"
    " \n"
    "   // The stack is now empty, so if we pop again the actor replies with an error.\n"
    "   let assert Error(Nil) = process.call(my_actor, Pop, 10)\n"
    " \n"
    "   // Lastly, we can send a message to the actor asking it to shut down.\n"
    "   process.send(my_actor, Shutdown)\n"
    " }\n"
    " ```\n"
    "\n"
    " Here is the code that is used to implement this actor:\n"
    "\n"
    " ```gleam\n"
    " import gleam/erlang/process.{type Subject}\n"
    " import gleam/otp/actor\n"
    "\n"
    " // First step of implementing the stack Actor is to define the message type that\n"
    " // it can receive.\n"
    " //\n"
    " // The type of the elements in the stack is not fixed so a type parameter is used\n"
    " // for it instead of a concrete type such as `String` or `Int`.\n"
    " pub type Message(element) {\n"
    "   // The `Shutdown` message is used to tell the actor to stop.\n"
    "   // It is the simplest message type, it contains no data.\n"
    "   Shutdown\n"
    " \n"
    "   // The `Push` message is used to add a new element to the stack.\n"
    "   // It contains the item to add, the type of which is the `element`\n"
    "   // parameterised type.\n"
    "   Push(push: element)\n"
    " \n"
    "   // The `Pop` message is used to remove an element from the stack.\n"
    "   // It contains a `Subject`, which is used to send the response back to the\n"
    "   // message sender. In this case the reply is of type `Result(element, Nil)`.\n"
    "   Pop(reply_with: Subject(Result(element, Nil)))\n"
    " }\n"
    " \n"
    " // The last part is to implement the `handle_message` callback function.\n"
    " //\n"
    " // This function is called by the Actor for each message it receives.\n"
    " // Actor is single threaded and only does one thing at a time, so it handles\n"
    " // messages sequentially and one at a time, in the order they are received.\n"
    " //\n"
    " // The function takes the message and the current state, and returns a data\n"
    " // structure that indicates what to do next, along with the new state.\n"
    " fn handle_message(\n"
    "  message: Message(e),\n"
    "  stack: List(e),\n"
    " ) -> actor.Next(Message(e), List(e)) {\n"
    "   case message {\n"
    "     // For the `Shutdown` message we return the `actor.Stop` value, which causes\n"
    "     // the actor to discard any remaining messages and stop.\n"
    "     Shutdown -> actor.Stop(process.Normal)\n"
    " \n"
    "     // For the `Push` message we add the new element to the stack and return\n"
    "     // `actor.continue` with this new stack, causing the actor to process any\n"
    "     // queued messages or wait for more.\n"
    "     Push(value) -> {\n"
    "       let new_state = [value, ..stack]\n"
    "       actor.continue(new_state)\n"
    "     }\n"
    " \n"
    "     // For the `Pop` message we attempt to remove an element from the stack,\n"
    "     // sending it or an error back to the caller, before continuing.\n"
    "     Pop(client) ->\n"
    "       case stack {\n"
    "         [] -> {\n"
    "           // When the stack is empty we can't pop an element, so we send an\n"
    "           // error back.\n"
    "           process.send(client, Error(Nil))\n"
    "           actor.continue([])\n"
    "         }\n"
    " \n"
    "         [first, ..rest] -> {\n"
    "           // Otherwise we send the first element back and use the remaining\n"
    "           // elements as the new state.\n"
    "           process.send(client, Ok(first))\n"
    "           actor.continue(rest)\n"
    "         }\n"
    "       }\n"
    "   }\n"
    " }\n"
    " ```\n"
).

-type message(GJL) :: {message, GJL} |
    {system, gleam@otp@system:system_message()} |
    {unexpected, gleam@dynamic:dynamic_()}.

-type next(GJM, GJN) :: {continue,
        GJN,
        gleam@option:option(gleam@erlang@process:selector(GJM))} |
    {stop, gleam@erlang@process:exit_reason()}.

-type init_result(GJO, GJP) :: {ready, GJO, gleam@erlang@process:selector(GJP)} |
    {failed, binary()}.

-type self(GJQ, GJR) :: {self,
        gleam@otp@system:mode(),
        gleam@erlang@process:pid_(),
        GJQ,
        gleam@erlang@process:subject(GJR),
        gleam@erlang@process:selector(message(GJR)),
        gleam@otp@system:debug_state(),
        fun((GJR, GJQ) -> next(GJR, GJQ))}.

-type spec(GJS, GJT) :: {spec,
        fun(() -> init_result(GJS, GJT)),
        integer(),
        fun((GJT, GJS) -> next(GJT, GJS))}.

-type start_error() :: init_timeout |
    {init_failed, gleam@erlang@process:exit_reason()} |
    {init_crashed, gleam@dynamic:dynamic_()}.

-type start_init_message(GJU) :: {ack,
        {ok, gleam@erlang@process:subject(GJU)} |
            {error, gleam@erlang@process:exit_reason()}} |
    {mon, gleam@erlang@process:process_down()}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 178).
-spec continue(GKB) -> next(any(), GKB).
continue(State) ->
    {continue, State, none}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 186).
?DOC(
    " Provide a selector to change the messages that the actor is handling\n"
    " going forward. This replaces any selector that was previously given\n"
    " in the actor's `init` callback, or in any previous `Next` value.\n"
).
-spec with_selector(next(GKF, GKG), gleam@erlang@process:selector(GKF)) -> next(GKF, GKG).
with_selector(Value, Selector) ->
    case Value of
        {continue, State, _} ->
            {continue, State, {some, Selector}};

        _ ->
            Value
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 259).
-spec exit_process(gleam@erlang@process:exit_reason()) -> gleam@erlang@process:exit_reason().
exit_process(Reason) ->
    Reason.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 295).
-spec selecting_system_messages(gleam@erlang@process:selector(message(GKR))) -> gleam@erlang@process:selector(message(GKR)).
selecting_system_messages(Selector) ->
    _pipe = Selector,
    gleam@erlang@process:selecting_record3(
        _pipe,
        erlang:binary_to_atom(<<"system"/utf8>>),
        fun gleam_otp_external:convert_system_message/2
    ).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 264).
-spec receive_message(self(any(), GKN)) -> message(GKN).
receive_message(Self) ->
    Selector = case erlang:element(2, Self) of
        suspended ->
            _pipe = gleam_erlang_ffi:new_selector(),
            selecting_system_messages(_pipe);

        running ->
            _pipe@1 = gleam_erlang_ffi:new_selector(),
            _pipe@2 = gleam@erlang@process:selecting_anything(
                _pipe@1,
                fun(Field@0) -> {unexpected, Field@0} end
            ),
            _pipe@3 = gleam_erlang_ffi:merge_selector(
                _pipe@2,
                erlang:element(6, Self)
            ),
            selecting_system_messages(_pipe@3)
    end,
    gleam_erlang_ffi:select(Selector).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 308).
-spec process_status_info(self(any(), any())) -> gleam@otp@system:status_info().
process_status_info(Self) ->
    {status_info,
        erlang:binary_to_atom(<<"gleam@otp@actor"/utf8>>),
        erlang:element(3, Self),
        erlang:element(2, Self),
        erlang:element(7, Self),
        gleam_stdlib:identity(erlang:element(4, Self))}.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 415).
-spec init_selector(
    gleam@erlang@process:subject(GPR),
    gleam@erlang@process:selector(GPR)
) -> gleam@erlang@process:selector(message(GPR)).
init_selector(Subject, Selector) ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = gleam@erlang@process:selecting(
        _pipe,
        Subject,
        fun(Field@0) -> {message, Field@0} end
    ),
    gleam_erlang_ffi:merge_selector(
        _pipe@1,
        gleam_erlang_ffi:map_selector(
            Selector,
            fun(Field@0) -> {message, Field@0} end
        )
    ).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 318).
-spec loop(self(any(), any())) -> gleam@erlang@process:exit_reason().
loop(Self) ->
    case receive_message(Self) of
        {system, System} ->
            case System of
                {get_state, Callback} ->
                    Callback(gleam_stdlib:identity(erlang:element(4, Self))),
                    loop(Self);

                {resume, Callback@1} ->
                    Callback@1(),
                    loop(
                        begin
                            _record = Self,
                            {self,
                                running,
                                erlang:element(3, _record),
                                erlang:element(4, _record),
                                erlang:element(5, _record),
                                erlang:element(6, _record),
                                erlang:element(7, _record),
                                erlang:element(8, _record)}
                        end
                    );

                {suspend, Callback@2} ->
                    Callback@2(),
                    loop(
                        begin
                            _record@1 = Self,
                            {self,
                                suspended,
                                erlang:element(3, _record@1),
                                erlang:element(4, _record@1),
                                erlang:element(5, _record@1),
                                erlang:element(6, _record@1),
                                erlang:element(7, _record@1),
                                erlang:element(8, _record@1)}
                        end
                    );

                {get_status, Callback@3} ->
                    Callback@3(process_status_info(Self)),
                    loop(Self)
            end;

        {unexpected, Message} ->
            logger:warning(
                unicode:characters_to_list(
                    <<"Actor discarding unexpected message: ~s"/utf8>>
                ),
                [unicode:characters_to_list(gleam@string:inspect(Message))]
            ),
            loop(Self);

        {message, Msg} ->
            case (erlang:element(8, Self))(Msg, erlang:element(4, Self)) of
                {stop, Reason} ->
                    exit_process(Reason);

                {continue, State, New_selector} ->
                    Selector = begin
                        _pipe = New_selector,
                        _pipe@1 = gleam@option:map(
                            _pipe,
                            fun(_capture) ->
                                init_selector(erlang:element(5, Self), _capture)
                            end
                        ),
                        gleam@option:unwrap(_pipe@1, erlang:element(6, Self))
                    end,
                    loop(
                        begin
                            _record@2 = Self,
                            {self,
                                erlang:element(2, _record@2),
                                erlang:element(3, _record@2),
                                State,
                                erlang:element(5, _record@2),
                                Selector,
                                erlang:element(7, _record@2),
                                erlang:element(8, _record@2)}
                        end
                    )
            end
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 374).
-spec initialise_actor(
    spec(any(), GLI),
    gleam@erlang@process:subject({ok, gleam@erlang@process:subject(GLI)} |
        {error, gleam@erlang@process:exit_reason()})
) -> gleam@erlang@process:exit_reason().
initialise_actor(Spec, Ack) ->
    Subject = gleam@erlang@process:new_subject(),
    Result = (erlang:element(2, Spec))(),
    case Result of
        {ready, State, Selector} ->
            Selector@1 = init_selector(Subject, Selector),
            gleam@erlang@process:send(Ack, {ok, Subject}),
            Self = {self,
                running,
                gleam@erlang@process:subject_owner(Ack),
                State,
                Subject,
                Selector@1,
                sys:debug_options([]),
                erlang:element(4, Spec)},
            loop(Self);

        {failed, Reason} ->
            gleam@erlang@process:send(Ack, {error, {abnormal, Reason}}),
            exit_process({abnormal, Reason})
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 444).
?DOC(
    " Convert a Gleam actor start result into an Erlang supervisor-compatible\n"
    " process start result.\n"
).
-spec to_erlang_start_result(
    {ok, gleam@erlang@process:subject(any())} | {error, start_error()}
) -> {ok, gleam@erlang@process:pid_()} | {error, gleam@dynamic:dynamic_()}.
to_erlang_start_result(Res) ->
    case Res of
        {ok, X} ->
            {ok, gleam@erlang@process:subject_owner(X)};

        {error, X@1} ->
            {error, gleam_stdlib:identity(X@1)}
    end.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 466).
?DOC(
    " Start an actor from a given specification. If the actor's `init` function\n"
    " returns an error or does not return within `init_timeout` then an error is\n"
    " returned.\n"
    "\n"
    " If you do not need to specify the initialisation behaviour of your actor\n"
    " consider using the `start` function.\n"
).
-spec start_spec(spec(any(), GLV)) -> {ok, gleam@erlang@process:subject(GLV)} |
    {error, start_error()}.
start_spec(Spec) ->
    Ack_subject = gleam@erlang@process:new_subject(),
    Child = gleam@erlang@process:start(
        fun() -> initialise_actor(Spec, Ack_subject) end,
        true
    ),
    Monitor = gleam@erlang@process:monitor_process(Child),
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = gleam@erlang@process:selecting(
            _pipe,
            Ack_subject,
            fun(Field@0) -> {ack, Field@0} end
        ),
        gleam@erlang@process:selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Field@0) -> {mon, Field@0} end
        )
    end,
    Result = case gleam_erlang_ffi:select(Selector, erlang:element(3, Spec)) of
        {ok, {ack, {ok, Channel}}} ->
            {ok, Channel};

        {ok, {ack, {error, Reason}}} ->
            {error, {init_failed, Reason}};

        {ok, {mon, Down}} ->
            {error, {init_crashed, erlang:element(3, Down)}};

        {error, nil} ->
            gleam@erlang@process:unlink(Child),
            gleam@erlang@process:kill(Child),
            {error, init_timeout}
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    Result.

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 517).
?DOC(
    " Start an actor with a given initial state and message handling loop\n"
    " function.\n"
    "\n"
    " This function returns a `Result` but it will always be `Ok` so it is safe\n"
    " to use with `assert` if you are not starting this actor as part of a\n"
    " supervision tree.\n"
    "\n"
    " If you wish to configure the initialisation behaviour of a new actor see\n"
    " the `Spec` record and the `start_spec` function.\n"
).
-spec start(GMB, fun((GMC, GMB) -> next(GMC, GMB))) -> {ok,
        gleam@erlang@process:subject(GMC)} |
    {error, start_error()}.
start(State, Loop) ->
    start_spec(
        {spec,
            fun() -> {ready, State, gleam_erlang_ffi:new_selector()} end,
            5000,
            Loop}
    ).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 532).
?DOC(
    " Send a message over a given channel.\n"
    "\n"
    " This is a re-export of `process.send`, for the sake of convenience.\n"
).
-spec send(gleam@erlang@process:subject(GMI), GMI) -> nil.
send(Subject, Msg) ->
    gleam@erlang@process:send(Subject, Msg).

-file("/Users/louis/src/gleam/otp/src/gleam/otp/actor.gleam", 545).
?DOC(
    " Send a synchronous message and wait for a response from the receiving\n"
    " process.\n"
    "\n"
    " If a reply is not received within the given timeout then the sender process\n"
    " crashes. If you wish to receive a `Result` rather than crashing see the\n"
    " `process.try_call` function.\n"
    "\n"
    " This is a re-export of `process.call`, for the sake of convenience.\n"
).
-spec call(
    gleam@erlang@process:subject(GMK),
    fun((gleam@erlang@process:subject(GMM)) -> GMK),
    integer()
) -> GMM.
call(Subject, Make_message, Timeout) ->
    gleam@erlang@process:call(Subject, Make_message, Timeout).
