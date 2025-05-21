-module(lustre_dev_tools@project).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([otp_version/0, build/0, root/0, config/0, type_to_string/1, interface/0]).
-export_type([config/0, interface/0, module_/0, function_/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type config() :: {config, binary(), gleam@dict:dict(binary(), tom:toml())}.

-type interface() :: {interface,
        binary(),
        binary(),
        gleam@dict:dict(binary(), module_())}.

-type module_() :: {module,
        gleam@dict:dict(binary(), gleam@package_interface:type()),
        gleam@dict:dict(binary(), function_())}.

-type function_() :: {function,
        list(gleam@package_interface:type()),
        gleam@package_interface:type()}.

-file("src/lustre_dev_tools/project.gleam", 38).
?DOC(false).
-spec otp_version() -> integer().
otp_version() ->
    Version = lustre_dev_tools_ffi:otp_version(),
    case gleam_stdlib:parse_int(Version) of
        {ok, Version@1} ->
            Version@1;

        {error, _} ->
            erlang:error(#{gleam_error => panic,
                    message => (<<"unexpected version number format: "/utf8,
                        Version/binary>>),
                    module => <<"lustre_dev_tools/project"/utf8>>,
                    function => <<"otp_version"/utf8>>,
                    line => 42})
    end.

-file("src/lustre_dev_tools/project.gleam", 51).
?DOC(false).
-spec build() -> {ok, nil} | {error, lustre_dev_tools@error:error()}.
build() ->
    _pipe = lustre_dev_tools_ffi:exec(
        <<"gleam"/utf8>>,
        [<<"build"/utf8>>, <<"--target"/utf8>>, <<"javascript"/utf8>>],
        <<"."/utf8>>
    ),
    _pipe@1 = gleam@result:map_error(
        _pipe,
        fun(Err) -> {build_error, gleam@pair:second(Err)} end
    ),
    gleam@result:replace(_pipe@1, nil).

-file("src/lustre_dev_tools/project.gleam", 100).
?DOC(false).
-spec find_root(binary()) -> binary().
find_root(Path) ->
    Toml = filepath:join(Path, <<"gleam.toml"/utf8>>),
    case simplifile_erl:is_file(Toml) of
        {ok, false} ->
            find_root(filepath:join(<<".."/utf8>>, Path));

        {error, _} ->
            find_root(filepath:join(<<".."/utf8>>, Path));

        {ok, true} ->
            Path
    end.

-file("src/lustre_dev_tools/project.gleam", 96).
?DOC(false).
-spec root() -> binary().
root() ->
    find_root(<<"."/utf8>>).

-file("src/lustre_dev_tools/project.gleam", 75).
?DOC(false).
-spec config() -> {ok, config()} | {error, lustre_dev_tools@error:error()}.
config() ->
    Configuration_path = filepath:join(root(), <<"gleam.toml"/utf8>>),
    _assert_subject = simplifile:read(Configuration_path),
    {ok, Configuration} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 84})
    end,
    _assert_subject@1 = tom:parse(Configuration),
    {ok, Toml} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@1,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 85})
    end,
    _assert_subject@2 = tom:get_string(Toml, [<<"name"/utf8>>]),
    {ok, Name} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail@2,
                        module => <<"lustre_dev_tools/project"/utf8>>,
                        function => <<"config"/utf8>>,
                        line => 86})
    end,
    {ok, {config, Name, Toml}}.

-file("src/lustre_dev_tools/project.gleam", 109).
?DOC(false).
-spec type_to_string(gleam@package_interface:type()) -> binary().
type_to_string(Type_) ->
    case Type_ of
        {tuple, Elements} ->
            Elements@1 = gleam@list:map(Elements, fun type_to_string/1),
            <<<<"#("/utf8,
                    (gleam@string:join(Elements@1, <<", "/utf8>>))/binary>>/binary,
                ")"/utf8>>;

        {fn, Params, Return} ->
            Params@1 = gleam@list:map(Params, fun type_to_string/1),
            Return@1 = type_to_string(Return),
            <<<<<<"fn("/utf8,
                        (gleam@string:join(Params@1, <<", "/utf8>>))/binary>>/binary,
                    ") -> "/utf8>>/binary,
                Return@1/binary>>;

        {named, Name, _, _, []} ->
            Name;

        {named, Name@1, _, _, Params@2} ->
            Params@3 = gleam@list:map(Params@2, fun type_to_string/1),
            <<<<<<Name@1/binary, "("/utf8>>/binary,
                    (gleam@string:join(Params@3, <<", "/utf8>>))/binary>>/binary,
                ")"/utf8>>;

        {variable, Id} ->
            <<"a_"/utf8, (erlang:integer_to_binary(Id))/binary>>
    end.

-file("src/lustre_dev_tools/project.gleam", 162).
?DOC(false).
-spec labelled_argument_decoder() -> gleam@dynamic@decode:decoder(gleam@package_interface:type()).
labelled_argument_decoder() ->
    gleam@dynamic@decode:at(
        [<<"type"/utf8>>],
        gleam@package_interface:type_decoder()
    ).

-file("src/lustre_dev_tools/project.gleam", 152).
?DOC(false).
-spec function_decoder() -> gleam@dynamic@decode:decoder(function_()).
function_decoder() ->
    gleam@dynamic@decode:field(
        <<"parameters"/utf8>>,
        gleam@dynamic@decode:list(labelled_argument_decoder()),
        fun(Parameters) ->
            gleam@dynamic@decode:field(
                <<"return"/utf8>>,
                gleam@package_interface:type_decoder(),
                fun(Return) ->
                    gleam@dynamic@decode:success({function, Parameters, Return})
                end
            )
        end
    ).

-file("src/lustre_dev_tools/project.gleam", 168).
?DOC(false).
-spec string_dict(gleam@dynamic@decode:decoder(XMU)) -> gleam@dynamic@decode:decoder(gleam@dict:dict(binary(), XMU)).
string_dict(Values) ->
    gleam@dynamic@decode:dict(
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        Values
    ).

-file("src/lustre_dev_tools/project.gleam", 142).
?DOC(false).
-spec module_decoder() -> gleam@dynamic@decode:decoder(module_()).
module_decoder() ->
    gleam@dynamic@decode:field(
        <<"constants"/utf8>>,
        string_dict(
            gleam@dynamic@decode:at(
                [<<"type"/utf8>>],
                gleam@package_interface:type_decoder()
            )
        ),
        fun(Constants) ->
            gleam@dynamic@decode:field(
                <<"functions"/utf8>>,
                string_dict(function_decoder()),
                fun(Functions) ->
                    gleam@dynamic@decode:success({module, Constants, Functions})
                end
            )
        end
    ).

-file("src/lustre_dev_tools/project.gleam", 134).
?DOC(false).
-spec interface_decoder() -> gleam@dynamic@decode:decoder(interface()).
interface_decoder() ->
    gleam@dynamic@decode:field(
        <<"name"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Name) ->
            gleam@dynamic@decode:field(
                <<"version"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Version) ->
                    gleam@dynamic@decode:field(
                        <<"modules"/utf8>>,
                        string_dict(module_decoder()),
                        fun(Modules) ->
                            gleam@dynamic@decode:success(
                                {interface, Name, Version, Modules}
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/lustre_dev_tools/project.gleam", 57).
?DOC(false).
-spec interface() -> {ok, interface()} | {error, lustre_dev_tools@error:error()}.
interface() ->
    Dir = filepath:join(root(), <<"build/.lustre"/utf8>>),
    Out = filepath:join(Dir, <<"package-interface.json"/utf8>>),
    Args = [<<"export"/utf8>>,
        <<"package-interface"/utf8>>,
        <<"--out"/utf8>>,
        Out],
    gleam@result:'try'(
        begin
            _pipe = lustre_dev_tools_ffi:exec(
                <<"gleam"/utf8>>,
                Args,
                <<"."/utf8>>
            ),
            gleam@result:map_error(
                _pipe,
                fun(Err) -> {build_error, gleam@pair:second(Err)} end
            )
        end,
        fun(_) ->
            _assert_subject = simplifile:read(Out),
            {ok, Json} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/project"/utf8>>,
                                function => <<"interface"/utf8>>,
                                line => 67})
            end,
            _assert_subject@1 = gleam@json:parse(Json, interface_decoder()),
            {ok, Interface} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail@1,
                                module => <<"lustre_dev_tools/project"/utf8>>,
                                function => <<"interface"/utf8>>,
                                line => 68})
            end,
            {ok, Interface}
        end
    ).
