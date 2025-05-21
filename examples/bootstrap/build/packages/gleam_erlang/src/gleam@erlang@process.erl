-module(gleam@erlang@process).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([self/0, start/2, new_subject/0, subject_owner/1, send/2, 'receive'/2, receive_forever/1, new_selector/0, select/2, select_forever/1, map_selector/2, merge_selector/2, flush_messages/0, selecting_trapped_exits/2, selecting/3, selecting_record2/3, selecting_record3/3, selecting_record4/3, selecting_record5/3, selecting_record6/3, selecting_record7/3, selecting_record8/3, selecting_anything/2, deselecting/2, sleep/1, sleep_forever/0, is_alive/1, monitor_process/1, selecting_process_down/3, demonitor_process/1, deselecting_process_down/2, try_call/3, call/3, try_call_forever/2, call_forever/2, link/1, unlink/1, send_after/3, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1, register/2, unregister/1, named/1, pid_from_dynamic/1]).
-export_type([pid_/0, subject/1, do_not_leak/0, selector/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, process_monitor/0, process_down/0, call_error/1, timer/0, cancelled/0, kill_flag/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type pid_() :: any().

-opaque subject(FBZ) :: {subject, pid_(), gleam@erlang:reference_()} |
    {gleam_phantom, FBZ}.

-type do_not_leak() :: any().

-type selector(FCA) :: any() | {gleam_phantom, FCA}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, binary()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-opaque process_monitor() :: {process_monitor, gleam@erlang:reference_()}.

-type process_down() :: {process_down, pid_(), gleam@dynamic:dynamic_()}.

-type call_error(FCB) :: {callee_down, gleam@dynamic:dynamic_()} |
    call_timeout |
    {gleam_phantom, FCB}.

-type timer() :: any().

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 15).
?DOC(" Get the `Pid` for the current process.\n").
-spec self() -> pid_().
self() ->
    erlang:self().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 29).
?DOC(
    " Create a new Erlang process that runs concurrently to the creator. In other\n"
    " languages this might be called a fibre, a green thread, or a coroutine.\n"
    "\n"
    " If `linked` is `True` then the created process is linked to the creator\n"
    " process. When a process terminates an exit signal is sent to all other\n"
    " processes that are linked to it, causing the process to either terminate or\n"
    " have to handle the signal.\n"
    "\n"
    " More can be read about processes and links in the [Erlang documentation][1].\n"
    "\n"
    " [1]: https://www.erlang.org/doc/reference_manual/processes.html\n"
).
-spec start(fun(() -> any()), boolean()) -> pid_().
start(Implementation, Link) ->
    case Link of
        true ->
            erlang:spawn_link(Implementation);

        false ->
            erlang:spawn(Implementation)
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 71).
?DOC(" Create a new `Subject` owned by the current process.\n").
-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), erlang:make_ref()}.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 78).
?DOC(
    " Get the owner process for a `Subject`. This is the process that created the\n"
    " `Subject` and will receive messages sent with it.\n"
).
-spec subject_owner(subject(any())) -> pid_().
subject_owner(Subject) ->
    erlang:element(2, Subject).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 111).
?DOC(
    " Send a message to a process using a `Subject`. The message must be of the\n"
    " type that the `Subject` accepts.\n"
    "\n"
    " This function does not wait for the `Subject` owner process to call the\n"
    " `receive` function, instead it returns once the message has been placed in\n"
    " the process' mailbox.\n"
    "\n"
    " # Ordering\n"
    "\n"
    " If process P1 sends two messages to process P2 it is guaranteed that process\n"
    " P1 will receive the messages in the order they were sent.\n"
    "\n"
    " If you wish to receive the messages in a different order you can send them\n"
    " on two different subjects and the receiver function can call the `receive`\n"
    " function for each subject in the desired order, or you can write some Erlang\n"
    " code to perform a selective receive.\n"
    "\n"
    " # Examples\n"
    "\n"
    " ```gleam\n"
    " let subject = new_subject()\n"
    " send(subject, \"Hello, Joe!\")\n"
    " ```\n"
).
-spec send(subject(FCK), FCK) -> nil.
send(Subject, Message) ->
    erlang:send(
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 132).
?DOC(
    " Receive a message that has been sent to current process using the `Subject`.\n"
    "\n"
    " If there is not an existing message for the `Subject` in the process'\n"
    " mailbox or one does not arrive `within` the permitted timeout then the\n"
    " `Error(Nil)` is returned.\n"
    "\n"
    " Only the process that is owner of the `Subject` can receive a message using\n"
    " it. If a process that does not own the `Subject` attempts to receive with it\n"
    " then it will not receive a message.\n"
    "\n"
    " To wait for messages from multiple `Subject`s at the same time see the\n"
    " `Selector` type.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
).
-spec 'receive'(subject(FCM), integer()) -> {ok, FCM} | {error, nil}.
'receive'(Subject, Timeout) ->
    gleam_erlang_ffi:'receive'(Subject, Timeout).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 141).
?DOC(
    " Receive a message that has been sent to current process using the `Subject`.\n"
    "\n"
    " Same as `receive` but waits forever and returns the message as is.\n"
).
-spec receive_forever(subject(FCQ)) -> FCQ.
receive_forever(Subject) ->
    gleam_erlang_ffi:'receive'(Subject).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 170).
?DOC(
    " Create a new `Selector` which can be used to receive messages on multiple\n"
    " `Subject`s at once.\n"
).
-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 190).
?DOC(
    " Receive a message that has been sent to current process using any of the\n"
    " `Subject`s that have been added to the `Selector` with the `selecting`\n"
    " function.\n"
    "\n"
    " If there is not an existing message for the `Selector` in the process'\n"
    " mailbox or one does not arrive `within` the permitted timeout then the\n"
    " `Error(Nil)` is returned.\n"
    "\n"
    " Only the process that is owner of the `Subject`s can receive a message using\n"
    " them. If a process that does not own the a `Subject` attempts to receive\n"
    " with it then it will not receive a message.\n"
    "\n"
    " To wait forever for the next message rather than for a limited amount of\n"
    " time see the `select_forever` function.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
).
-spec select(selector(FCU), integer()) -> {ok, FCU} | {error, nil}.
select(From, Within) ->
    gleam_erlang_ffi:select(From, Within).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 199).
?DOC(
    " Similar to the `select` function but will wait forever for a message to\n"
    " arrive rather than timing out after a specified amount of time.\n"
).
-spec select_forever(selector(FCY)) -> FCY.
select_forever(From) ->
    gleam_erlang_ffi:select(From).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 208).
?DOC(
    " Add a transformation function to a selector. When a message is received\n"
    " using this selector the transformation function is applied to the message.\n"
    "\n"
    " This function can be used to change the type of messages received and may\n"
    " be useful when combined with the `merge_selector` function.\n"
).
-spec map_selector(selector(FDA), fun((FDA) -> FDC)) -> selector(FDC).
map_selector(A, B) ->
    gleam_erlang_ffi:map_selector(A, B).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 217).
?DOC(
    " Merge one selector into another, producing a selector that contains the\n"
    " message handlers of both.\n"
    "\n"
    " If a subject is handled by both selectors the handler function of the\n"
    " second selector is used.\n"
).
-spec merge_selector(selector(FDE), selector(FDE)) -> selector(FDE).
merge_selector(A, B) ->
    gleam_erlang_ffi:merge_selector(A, B).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 261).
?DOC(
    " Discard all messages in the current process' mailbox.\n"
    "\n"
    " Warning: This function may cause other processes to crash if they sent a\n"
    " message to the current process and are waiting for a response, so use with\n"
    " caution.\n"
).
-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 233).
?DOC(
    " Add a handler for trapped exit messages. In order for these messages to be\n"
    " sent to the process when a linked process exits the process must call the\n"
    " `trap_exit` beforehand.\n"
).
-spec selecting_trapped_exits(selector(FDI), fun((exit_message()) -> FDI)) -> selector(FDI).
selecting_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Reason = erlang:element(3, Message),
        Normal = gleam_stdlib:identity(normal),
        Killed = gleam_stdlib:identity(killed),
        Reason@2 = case gleam@dynamic@decode:run(
            Reason,
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ) of
            _ when Reason =:= Normal ->
                normal;

            _ when Reason =:= Killed ->
                killed;

            {ok, Reason@1} ->
                {abnormal, Reason@1};

            {error, _} ->
                {abnormal, gleam@string:inspect(Reason)}
        end,
        Handler({exit_message, erlang:element(2, Message), Reason@2})
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 274).
?DOC(
    " Add a new `Subject` to the `Selector` so that its messages can be selected\n"
    " from the receiver process inbox.\n"
    "\n"
    " The `mapping` function provided with the `Subject` can be used to convert\n"
    " the type of messages received using this `Subject`. This is useful for when\n"
    " you wish to add multiple `Subject`s to a `Selector` when they have differing\n"
    " message types. If you do not wish to transform the incoming messages in any\n"
    " way then the `identity` function can be given.\n"
    "\n"
    " See `deselecting` to remove a subject from a selector.\n"
).
-spec selecting(selector(FDL), subject(FDN), fun((FDN) -> FDL)) -> selector(FDL).
selecting(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2},
        Handler
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 300).
?DOC(
    " Add a handler to a selector for 2 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record2(
    selector(FDV),
    any(),
    fun((gleam@dynamic:dynamic_()) -> FDV)
) -> selector(FDV).
selecting_record2(Selector, Tag, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 2}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 316).
?DOC(
    " Add a handler to a selector for 3 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record3(
    selector(FDZ),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FDZ)
) -> selector(FDZ).
selecting_record3(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(erlang:element(2, Message), erlang:element(3, Message))
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 334).
?DOC(
    " Add a handler to a selector for 4 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record4(
    selector(FED),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FED)
) -> selector(FED).
selecting_record4(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 4}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 352).
?DOC(
    " Add a handler to a selector for 5 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record5(
    selector(FEH),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FEH)
) -> selector(FEH).
selecting_record5(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 5}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 370).
?DOC(
    " Add a handler to a selector for 6 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record6(
    selector(FEL),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FEL)
) -> selector(FEL).
selecting_record6(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 6}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 388).
?DOC(
    " Add a handler to a selector for 7 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record7(
    selector(FEP),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FEP)
) -> selector(FEP).
selecting_record7(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 7}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 409).
?DOC(
    " Add a handler to a selector for 8 element tuple messages with a given tag\n"
    " element in the first position.\n"
    "\n"
    " Typically you want to use the `selecting` function with a `Subject` instead,\n"
    " but this function may be useful if you need to receive messages sent from\n"
    " other BEAM languages that do not use the `Subject` type.\n"
).
-spec selecting_record8(
    selector(FET),
    any(),
    fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> FET)
) -> selector(FET).
selecting_record8(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message),
            erlang:element(8, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 8}, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 459).
?DOC(
    " Add a catch-all handler to a selector that will be used when no other\n"
    " handler in a selector is suitable for a given message.\n"
    "\n"
    " This may be useful for when you want to ensure that any message in the inbox\n"
    " is handled, or when you need to handle messages from other BEAM languages\n"
    " which do not use subjects or record format messages.\n"
).
-spec selecting_anything(selector(FEX), fun((gleam@dynamic:dynamic_()) -> FEX)) -> selector(FEX).
selecting_anything(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 286).
?DOC(
    " Remove a new `Subject` from the `Selector` so that its messages will not be\n"
    " selected from the receiver process inbox.\n"
).
-spec deselecting(selector(FDQ), subject(any())) -> selector(FDQ).
deselecting(Selector, Subject) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2}
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 483).
?DOC(
    " Suspends the process calling this function for the specified number of\n"
    " milliseconds.\n"
).
-spec sleep(integer()) -> nil.
sleep(A) ->
    gleam_erlang_ffi:sleep(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 490).
?DOC(
    " Suspends the process forever! This may be useful for suspending the main\n"
    " process in a Gleam program when it has no more work to do but we want other\n"
    " processes to continue to work.\n"
).
-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 499).
?DOC(
    " Check to see whether the process for a given `Pid` is alive.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#is_process_alive-1\n"
).
-spec is_alive(pid_()) -> boolean().
is_alive(A) ->
    erlang:is_process_alive(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 530).
?DOC(
    " Start monitoring a process so that when the monitored process exits a\n"
    " message is sent to the monitoring process.\n"
    "\n"
    " The message is only sent once, when the target process exits. If the\n"
    " process was not alive when this function is called the message will never\n"
    " be received.\n"
    "\n"
    " The down message can be received with a `Selector` and the\n"
    " `selecting_process_down` function.\n"
    "\n"
    " The process can be demonitored with the `demonitor_process` function.\n"
).
-spec monitor_process(pid_()) -> process_monitor().
monitor_process(Pid) ->
    _pipe = process,
    _pipe@1 = erlang:monitor(_pipe, Pid),
    {process_monitor, _pipe@1}.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 541).
?DOC(
    " Add a `ProcessMonitor` to a `Selector` so that the `ProcessDown` message can\n"
    " be received using the `Selector` and the `select` function. The\n"
    " `ProcessMonitor` can be removed later with\n"
    " [`deselecting_process_down`](#deselecting_process_down).\n"
).
-spec selecting_process_down(
    selector(FFJ),
    process_monitor(),
    fun((process_down()) -> FFJ)
) -> selector(FFJ).
selecting_process_down(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        erlang:element(2, Monitor),
        Mapping
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 555).
?DOC(
    " Remove the monitor for a process so that when the monitor process exits a\n"
    " `ProcessDown` message is not sent to the monitoring process.\n"
    "\n"
    " If the message has already been sent it is removed from the monitoring\n"
    " process' mailbox.\n"
).
-spec demonitor_process(process_monitor()) -> nil.
demonitor_process(Monitor) ->
    gleam_erlang_ffi:demonitor(Monitor),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 580).
?DOC(
    " Remove a `ProcessMonitor` from a `Selector` prevoiusly added by\n"
    " [`selecting_process_down`](#selecting_process_down). If the `ProcessMonitor` is not in the\n"
    " `Selector` it will be returned unchanged.\n"
).
-spec deselecting_process_down(selector(FFM), process_monitor()) -> selector(FFM).
deselecting_process_down(Selector, Monitor) ->
    gleam_erlang_ffi:remove_selector_handler(
        Selector,
        erlang:element(2, Monitor)
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 595).
?DOC(
    " Send a message to a process and wait for a reply.\n"
    "\n"
    " If the receiving process exits or does not reply within the allowed amount\n"
    " of time then an error is returned.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
).
-spec try_call(subject(FFP), fun((subject(FFR)) -> FFP), integer()) -> {ok, FFR} |
    {error, call_error(FFR)}.
try_call(Subject, Make_request, Timeout) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2, Timeout)
    end,
    demonitor_process(Monitor),
    case Result of
        {error, nil} ->
            {error, call_timeout};

        {ok, Res} ->
            Res
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 636).
?DOC(
    " Send a message to a process and wait for a reply.\n"
    "\n"
    " If the receiving process exits or does not reply within the allowed amount\n"
    " of time the calling process crashes. If you wish an error to be returned\n"
    " instead see the `try_call` function.\n"
    "\n"
    " The `within` parameter specifies the timeout duration in milliseconds.\n"
).
-spec call(subject(FFW), fun((subject(FFY)) -> FFW), integer()) -> FFY.
call(Subject, Make_request, Timeout) ->
    _assert_subject = try_call(Subject, Make_request, Timeout),
    {ok, Resp} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call"/utf8>>,
                        line => 641})
    end,
    Resp.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 665).
?DOC(
    " Similar to the `try_call` function but will wait forever for a message\n"
    " to arrive rather than timing out after a specified amount of time.\n"
    "\n"
    " If the receiving process exits then an error is returned.\n"
).
-spec try_call_forever(subject(FGE), fun((subject(FGG)) -> FGE)) -> {ok, FGG} |
    {error, call_error(any())}.
try_call_forever(Subject, Make_request) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2)
    end,
    demonitor_process(Monitor),
    Result.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 652).
?DOC(
    " Similar to the `call` function but will wait forever for a message to\n"
    " arrive rather than timing out after a specified amount of time.\n"
    "\n"
    " If the receiving process exits, the calling process crashes.\n"
    " If you wish an error to be returned instead see the `try_call_forever`\n"
    " function.\n"
).
-spec call_forever(subject(FGA), fun((subject(FGC)) -> FGA)) -> FGC.
call_forever(Subject, Make_request) ->
    _assert_subject = try_call_forever(Subject, Make_request),
    {ok, Response} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call_forever"/utf8>>,
                        line => 656})
    end,
    Response.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 703).
?DOC(
    " Creates a link between the calling process and another process.\n"
    "\n"
    " When a process crashes any linked processes will also crash. This is useful\n"
    " to ensure that groups of processes that depend on each other all either\n"
    " succeed or fail together.\n"
    "\n"
    " Returns `True` if the link was created successfully, returns `False` if the\n"
    " process was not alive and as such could not be linked.\n"
).
-spec link(pid_()) -> boolean().
link(Pid) ->
    gleam_erlang_ffi:link(Pid).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 710).
?DOC(" Removes any existing link between the caller process and the target process.\n").
-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 722).
?DOC(" Send a message over a channel after a specified number of milliseconds.\n").
-spec send_after(subject(FGN), integer(), FGN) -> timer().
send_after(Subject, Delay, Message) ->
    erlang:send_after(
        Delay,
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 746).
?DOC(" Cancel a given timer, causing it not to trigger if it has not done already.\n").
-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic@decode:run(
        erlang:cancel_timer(Timer),
        {decoder, fun gleam@dynamic@decode:decode_int/1}
    ) of
        {ok, I} ->
            {cancelled, I};

        {error, _} ->
            timer_not_found
    end.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 770).
?DOC(
    " Send an untrappable `kill` exit signal to the target process.\n"
    "\n"
    " See the documentation for the Erlang [`erlang:exit`][1] function for more\n"
    " information.\n"
    "\n"
    " [1]: https://erlang.org/doc/man/erlang.html#exit-1\n"
).
-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 786).
?DOC(
    " Sends an exit signal to a process, indicating that the process is to shut\n"
    " down.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#exit-2\n"
).
-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 798).
?DOC(
    " Sends an exit signal to a process, indicating that the process is to shut\n"
    " down due to an abnormal reason such as a failure.\n"
    "\n"
    " See the [Erlang documentation][1] for more information.\n"
    "\n"
    " [1]: http://erlang.org/doc/man/erlang.html#exit-2\n"
).
-spec send_abnormal_exit(pid_(), binary()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, {abnormal, Reason}),
    nil.

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 814).
?DOC(
    " Set whether the current process is to trap exit signals or not.\n"
    "\n"
    " When not trapping exits if a linked process crashes the exit signal\n"
    " propagates to the process which will also crash.\n"
    " This is the normal behaviour before this function is called.\n"
    "\n"
    " When trapping exits (after this function is called) if a linked process\n"
    " crashes an exit message is sent to the process instead. These messages can\n"
    " be handled with the `selecting_trapped_exits` function.\n"
).
-spec trap_exits(boolean()) -> nil.
trap_exits(A) ->
    gleam_erlang_ffi:trap_exits(A).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 826).
?DOC(
    " Register a process under a given name, allowing it to be looked up using\n"
    " the `named` function.\n"
    "\n"
    " This function will return an error under the following conditions:\n"
    " - The process for the pid no longer exists.\n"
    " - The name has already been registered.\n"
    " - The process already has a name.\n"
    " - The name is the atom `undefined`, which is reserved by Erlang.\n"
).
-spec register(pid_(), gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
register(Pid, Name) ->
    gleam_erlang_ffi:register_process(Pid, Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 837).
?DOC(
    " Un-register a process name, after which the process can no longer be looked\n"
    " up by that name, and both the name and the process can be re-used in other\n"
    " registrations.\n"
    "\n"
    " It is possible to un-register process that are not from your application,\n"
    " including those from Erlang/OTP itself. This is not recommended and will\n"
    " likely result in undesirable behaviour and crashes.\n"
).
-spec unregister(gleam@erlang@atom:atom_()) -> {ok, nil} | {error, nil}.
unregister(Name) ->
    gleam_erlang_ffi:unregister_process(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 842).
?DOC(" Look up a process by name, returning the pid if it exists.\n").
-spec named(gleam@erlang@atom:atom_()) -> {ok, pid_()} | {error, nil}.
named(Name) ->
    gleam_erlang_ffi:process_named(Name).

-file("/Users/louis/src/gleam/erlang/src/gleam/erlang/process.gleam", 860).
?DOC(
    " Checks to see whether a `Dynamic` value is a pid, and return the pid if\n"
    " it is.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " ```gleam\n"
    " import gleam/dynamic\n"
    " pid_from_dynamic(dynamic.from(process.self()))\n"
    " // -> Ok(process.self())\n"
    " ```\n"
    "\n"
    " ```gleam\n"
    " pid_from_dynamic(dynamic.from(123))\n"
    " // -> Error([DecodeError(expected: \"Pid\", found: \"Int\", path: [])])\n"
    " ```\n"
).
-spec pid_from_dynamic(gleam@dynamic:dynamic_()) -> {ok, pid_()} |
    {error, list(gleam@dynamic@decode:decode_error())}.
pid_from_dynamic(From) ->
    gleam_erlang_ffi:pid_from_dynamic(From).
