-module(gleam@package_interface).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([deprecation_decoder/0, implementations_decoder/0, type_decoder/0, type_alias_decoder/0, constant_decoder/0, parameter_decoder/0, function_decoder/0, constructor_decoder/0, type_definition_decoder/0, module_decoder/0, decoder/0]).
-export_type([package/0, module_/0, type_alias/0, type_definition/0, type_constructor/0, parameter/0, constant/0, function_/0, deprecation/0, implementations/0, type/0]).

-type package() :: {package,
        binary(),
        binary(),
        gleam@option:option(binary()),
        gleam@dict:dict(binary(), module_())}.

-type module_() :: {module,
        list(binary()),
        gleam@dict:dict(binary(), type_alias()),
        gleam@dict:dict(binary(), type_definition()),
        gleam@dict:dict(binary(), constant()),
        gleam@dict:dict(binary(), function_())}.

-type type_alias() :: {type_alias,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        integer(),
        type()}.

-type type_definition() :: {type_definition,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        integer(),
        list(type_constructor())}.

-type type_constructor() :: {type_constructor,
        gleam@option:option(binary()),
        binary(),
        list(parameter())}.

-type parameter() :: {parameter, gleam@option:option(binary()), type()}.

-type constant() :: {constant,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        implementations(),
        type()}.

-type function_() :: {function,
        gleam@option:option(binary()),
        gleam@option:option(deprecation()),
        implementations(),
        list(parameter()),
        type()}.

-type deprecation() :: {deprecation, binary()}.

-type implementations() :: {implementations,
        boolean(),
        boolean(),
        boolean(),
        boolean(),
        boolean()}.

-type type() :: {tuple, list(type())} |
    {fn, list(type()), type()} |
    {variable, integer()} |
    {named, binary(), binary(), binary(), list(type())}.

-file("src/gleam/package_interface.gleam", 461).
-spec deprecation_decoder() -> gleam@dynamic@decode:decoder(deprecation()).
deprecation_decoder() ->
    gleam@dynamic@decode:field(
        <<"message"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Message) -> gleam@dynamic@decode:success({deprecation, Message}) end
    ).

-file("src/gleam/package_interface.gleam", 476).
-spec implementations_decoder() -> gleam@dynamic@decode:decoder(implementations()).
implementations_decoder() ->
    gleam@dynamic@decode:field(
        <<"gleam"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_bool/1},
        fun(Gleam) ->
            gleam@dynamic@decode:field(
                <<"uses-erlang-externals"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_bool/1},
                fun(Uses_erlang_externals) ->
                    gleam@dynamic@decode:field(
                        <<"uses-javascript-externals"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_bool/1},
                        fun(Uses_javascript_externals) ->
                            gleam@dynamic@decode:optional_field(
                                <<"can-run-on-erlang"/utf8>>,
                                none,
                                gleam@dynamic@decode:optional(
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_bool/1}
                                ),
                                fun(Can_run_on_erlang) ->
                                    gleam@dynamic@decode:optional_field(
                                        <<"can-run-on-javascript"/utf8>>,
                                        none,
                                        gleam@dynamic@decode:optional(
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_bool/1}
                                        ),
                                        fun(Can_run_on_javascript) ->
                                            Can_run_on_erlang@1 = begin
                                                _pipe = Can_run_on_erlang,
                                                gleam@option:unwrap(
                                                    _pipe,
                                                    Gleam
                                                )
                                            end,
                                            Can_run_on_javascript@1 = begin
                                                _pipe@1 = Can_run_on_javascript,
                                                gleam@option:unwrap(
                                                    _pipe@1,
                                                    Gleam
                                                )
                                            end,
                                            gleam@dynamic@decode:success(
                                                {implementations,
                                                    Gleam,
                                                    Uses_erlang_externals,
                                                    Uses_javascript_externals,
                                                    Can_run_on_erlang@1,
                                                    Can_run_on_javascript@1}
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 513).
-spec type_decoder() -> gleam@dynamic@decode:decoder(type()).
type_decoder() ->
    gleam@dynamic@decode:field(
        <<"kind"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Kind) -> case Kind of
                <<"variable"/utf8>> ->
                    gleam@dynamic@decode:field(
                        <<"id"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(Id) ->
                            gleam@dynamic@decode:success({variable, Id})
                        end
                    );

                <<"tuple"/utf8>> ->
                    gleam@dynamic@decode:field(
                        <<"elements"/utf8>>,
                        gleam@dynamic@decode:list(type_decoder()),
                        fun(Elements) ->
                            gleam@dynamic@decode:success({tuple, Elements})
                        end
                    );

                <<"named"/utf8>> ->
                    gleam@dynamic@decode:field(
                        <<"name"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        fun(Name) ->
                            gleam@dynamic@decode:field(
                                <<"package"/utf8>>,
                                {decoder,
                                    fun gleam@dynamic@decode:decode_string/1},
                                fun(Package) ->
                                    gleam@dynamic@decode:field(
                                        <<"module"/utf8>>,
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_string/1},
                                        fun(Module) ->
                                            gleam@dynamic@decode:field(
                                                <<"parameters"/utf8>>,
                                                gleam@dynamic@decode:list(
                                                    type_decoder()
                                                ),
                                                fun(Parameters) ->
                                                    gleam@dynamic@decode:success(
                                                        {named,
                                                            Name,
                                                            Package,
                                                            Module,
                                                            Parameters}
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    );

                <<"fn"/utf8>> ->
                    gleam@dynamic@decode:field(
                        <<"parameters"/utf8>>,
                        gleam@dynamic@decode:list(type_decoder()),
                        fun(Parameters@1) ->
                            gleam@dynamic@decode:field(
                                <<"return"/utf8>>,
                                type_decoder(),
                                fun(Return) ->
                                    gleam@dynamic@decode:success(
                                        {fn, Parameters@1, Return}
                                    )
                                end
                            )
                        end
                    );

                _ ->
                    gleam@dynamic@decode:failure(
                        {variable, 0},
                        <<"String of variable, tuple, named, or fn"/utf8>>
                    )
            end end
    ).

-file("src/gleam/package_interface.gleam", 391).
-spec type_alias_decoder() -> gleam@dynamic@decode:decoder(type_alias()).
type_alias_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"deprecation"/utf8>>,
                gleam@dynamic@decode:optional(deprecation_decoder()),
                fun(Deprecation) ->
                    gleam@dynamic@decode:field(
                        <<"parameters"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(Parameters) ->
                            gleam@dynamic@decode:field(
                                <<"alias"/utf8>>,
                                type_decoder(),
                                fun(Alias) ->
                                    _pipe = {type_alias,
                                        Documentation,
                                        Deprecation,
                                        Parameters,
                                        Alias},
                                    gleam@dynamic@decode:success(_pipe)
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 424).
-spec constant_decoder() -> gleam@dynamic@decode:decoder(constant()).
constant_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"deprecation"/utf8>>,
                gleam@dynamic@decode:optional(deprecation_decoder()),
                fun(Deprecation) ->
                    gleam@dynamic@decode:field(
                        <<"implementations"/utf8>>,
                        implementations_decoder(),
                        fun(Implementations) ->
                            gleam@dynamic@decode:field(
                                <<"type"/utf8>>,
                                type_decoder(),
                                fun(Type_) ->
                                    _pipe = {constant,
                                        Documentation,
                                        Deprecation,
                                        Implementations,
                                        Type_},
                                    gleam@dynamic@decode:success(_pipe)
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 507).
-spec parameter_decoder() -> gleam@dynamic@decode:decoder(parameter()).
parameter_decoder() ->
    gleam@dynamic@decode:field(
        <<"label"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Label) ->
            gleam@dynamic@decode:field(
                <<"type"/utf8>>,
                type_decoder(),
                fun(Type_) ->
                    gleam@dynamic@decode:success({parameter, Label, Type_})
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 442).
-spec function_decoder() -> gleam@dynamic@decode:decoder(function_()).
function_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"deprecation"/utf8>>,
                gleam@dynamic@decode:optional(deprecation_decoder()),
                fun(Deprecation) ->
                    gleam@dynamic@decode:field(
                        <<"implementations"/utf8>>,
                        implementations_decoder(),
                        fun(Implementations) ->
                            gleam@dynamic@decode:field(
                                <<"parameters"/utf8>>,
                                gleam@dynamic@decode:list(parameter_decoder()),
                                fun(Parameters) ->
                                    gleam@dynamic@decode:field(
                                        <<"return"/utf8>>,
                                        type_decoder(),
                                        fun(Return) ->
                                            _pipe = {function,
                                                Documentation,
                                                Deprecation,
                                                Implementations,
                                                Parameters,
                                                Return},
                                            gleam@dynamic@decode:success(_pipe)
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 466).
-spec constructor_decoder() -> gleam@dynamic@decode:decoder(type_constructor()).
constructor_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"name"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Name) ->
                    gleam@dynamic@decode:field(
                        <<"parameters"/utf8>>,
                        gleam@dynamic@decode:list(parameter_decoder()),
                        fun(Parameters) ->
                            gleam@dynamic@decode:success(
                                {type_constructor,
                                    Documentation,
                                    Name,
                                    Parameters}
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 406).
-spec type_definition_decoder() -> gleam@dynamic@decode:decoder(type_definition()).
type_definition_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"deprecation"/utf8>>,
                gleam@dynamic@decode:optional(deprecation_decoder()),
                fun(Deprecation) ->
                    gleam@dynamic@decode:field(
                        <<"parameters"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(Parameters) ->
                            gleam@dynamic@decode:field(
                                <<"constructors"/utf8>>,
                                gleam@dynamic@decode:list(constructor_decoder()),
                                fun(Constructors) ->
                                    _pipe = {type_definition,
                                        Documentation,
                                        Deprecation,
                                        Parameters,
                                        Constructors},
                                    gleam@dynamic@decode:success(_pipe)
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 369).
-spec module_decoder() -> gleam@dynamic@decode:decoder(module_()).
module_decoder() ->
    gleam@dynamic@decode:field(
        <<"documentation"/utf8>>,
        gleam@dynamic@decode:list(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Documentation) ->
            gleam@dynamic@decode:field(
                <<"type-aliases"/utf8>>,
                gleam@dynamic@decode:dict(
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    type_alias_decoder()
                ),
                fun(Type_aliases) ->
                    gleam@dynamic@decode:field(
                        <<"types"/utf8>>,
                        gleam@dynamic@decode:dict(
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            type_definition_decoder()
                        ),
                        fun(Types) ->
                            gleam@dynamic@decode:field(
                                <<"constants"/utf8>>,
                                gleam@dynamic@decode:dict(
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    constant_decoder()
                                ),
                                fun(Constants) ->
                                    gleam@dynamic@decode:field(
                                        <<"functions"/utf8>>,
                                        gleam@dynamic@decode:dict(
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1},
                                            function_decoder()
                                        ),
                                        fun(Functions) ->
                                            _pipe = {module,
                                                Documentation,
                                                Type_aliases,
                                                Types,
                                                Constants,
                                                Functions},
                                            gleam@dynamic@decode:success(_pipe)
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/gleam/package_interface.gleam", 354).
-spec decoder() -> gleam@dynamic@decode:decoder(package()).
decoder() ->
    gleam@dynamic@decode:field(
        <<"name"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Name) ->
            gleam@dynamic@decode:field(
                <<"version"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Version) ->
                    gleam@dynamic@decode:field(
                        <<"gleam-version-constraint"/utf8>>,
                        gleam@dynamic@decode:optional(
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ),
                        fun(Gleam_version_constraint) ->
                            gleam@dynamic@decode:field(
                                <<"modules"/utf8>>,
                                gleam@dynamic@decode:dict(
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1},
                                    module_decoder()
                                ),
                                fun(Modules) ->
                                    _pipe = {package,
                                        Name,
                                        Version,
                                        Gleam_version_constraint,
                                        Modules},
                                    gleam@dynamic@decode:success(_pipe)
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
