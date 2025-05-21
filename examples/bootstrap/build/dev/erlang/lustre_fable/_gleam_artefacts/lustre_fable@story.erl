-module(lustre_fable@story).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([register/3]).
-export_type([story/0, story_builder/0, story_config/0, model/0, msg/0, scene_model/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type story() :: {story, binary(), binary(), binary(), binary()}.

-type story_builder() :: {story_builder, fun((integer()) -> story_config())}.

-type story_config() :: {story_config,
        binary(),
        list(fun((model()) -> lustre@vdom@vnode:element(msg()))),
        list(lustre@component:option(msg())),
        fun((model()) -> lustre@vdom@vnode:element(msg()))}.

-type model() :: {model, gleam@dict:dict(integer(), lustre_fable@value:value())}.

-type msg() :: {external_stylesheet_loaded,
        {ok, binary()} | {error, rsvp:error()}} |
    {component_updated_value, integer(), lustre_fable@value:value()} |
    {user_edited_value, integer(), lustre_fable@value:value()}.

-type scene_model() :: {scene_model,
        gleam@dict:dict(integer(), lustre_fable@value:value()),
        list(binary()),
        integer()}.

-file("src/lustre_fable/story.gleam", 102).
?DOC(false).
-spec story_init(any()) -> model().
story_init(_) ->
    {model, maps:new()}.

-file("src/lustre_fable/story.gleam", 106).
?DOC(false).
-spec story_update(model(), msg()) -> model().
story_update(Model, Msg) ->
    case Msg of
        {external_stylesheet_loaded, _} ->
            Model;

        {component_updated_value, Key, Value} ->
            {model, gleam@dict:insert(erlang:element(2, Model), Key, Value)};

        {user_edited_value, Key@1, Value@1} ->
            {model, gleam@dict:insert(erlang:element(2, Model), Key@1, Value@1)}
    end.

-file("src/lustre_fable/story.gleam", 118).
?DOC(false).
-spec story_view(
    model(),
    binary(),
    list(fun((model()) -> lustre@vdom@vnode:element(msg())))
) -> lustre@vdom@vnode:element(msg()).
story_view(Model, Scene, Inputs) ->
    Handle_change = begin
        gleam@dynamic@decode:subfield(
            [<<"detail"/utf8>>, <<"key"/utf8>>],
            {decoder, fun gleam@dynamic@decode:decode_int/1},
            fun(Key) ->
                gleam@dynamic@decode:subfield(
                    [<<"detail"/utf8>>, <<"value"/utf8>>],
                    lustre_fable@value:decoder(),
                    fun(Value) ->
                        gleam@dynamic@decode:success(
                            {component_updated_value, Key, Value}
                        )
                    end
                )
            end
        )
    end,
    Attributes@1 = gleam@dict:fold(
        erlang:element(2, Model),
        [],
        fun(Attributes, Key@1, Value@1) ->
            [lustre@attribute:property(
                    erlang:integer_to_binary(Key@1),
                    lustre_fable@value:to_json(Value@1)
                ) |
                Attributes]
        end
    ),
    lustre@element@html:'div'(
        [lustre@attribute:class(<<"@container h-full"/utf8>>)],
        [lustre@element@html:'div'(
                [lustre@attribute:class(
                        <<"h-full grid grid-cols-1 grid-rows-[1fr_300px]"/utf8>>
                    ),
                    lustre@attribute:class(
                        <<"@3xl:grid-cols-[1fr_300px] @3xl:grid-rows-1"/utf8>>
                    )],
                [lustre@element@html:main(
                        [lustre@attribute:class(
                                <<"p-4 grid grid-cols-1 grid-rows-1 place-items-center"/utf8>>
                            )],
                        [lustre@element:element(
                                Scene,
                                [lustre@event:on(
                                        <<"change"/utf8>>,
                                        Handle_change
                                    ) |
                                    Attributes@1],
                                []
                            )]
                    ),
                    lustre@element@html:aside(
                        [lustre@attribute:class(<<"p-4 border-t"/utf8>>),
                            lustre@attribute:class(
                                <<"@3xl:border-t-0 @3xl:border-l"/utf8>>
                            )],
                        gleam@list:map(Inputs, fun(Input) -> Input(Model) end)
                    )]
            )]
    ).

-file("src/lustre_fable/story.gleam", 181).
?DOC(false).
-spec scene_init(any(), list(binary()), list(binary())) -> {scene_model(),
    lustre@effect:effect(msg())}.
scene_init(_, Stylesheets, External_stylesheets) ->
    Model = {scene_model,
        maps:new(),
        Stylesheets,
        erlang:length(External_stylesheets)},
    Effect = begin
        _pipe = External_stylesheets,
        _pipe@1 = gleam@list:map(
            _pipe,
            fun(_capture) ->
                rsvp:get(
                    _capture,
                    rsvp:expect_text(
                        fun(Field@0) -> {external_stylesheet_loaded, Field@0} end
                    )
                )
            end
        ),
        lustre@effect:batch(_pipe@1)
    end,
    {Model, Effect}.

-file("src/lustre_fable/story.gleam", 201).
?DOC(false).
-spec scene_update(scene_model(), msg()) -> {scene_model(),
    lustre@effect:effect(msg())}.
scene_update(Model, Msg) ->
    case Msg of
        {component_updated_value, Key, Value} ->
            {begin
                    _record = Model,
                    {scene_model,
                        gleam@dict:insert(erlang:element(2, Model), Key, Value),
                        erlang:element(3, _record),
                        erlang:element(4, _record)}
                end,
                lustre@effect:none()};

        {external_stylesheet_loaded, {ok, Css}} ->
            Model@1 = begin
                _record@1 = Model,
                {scene_model,
                    erlang:element(2, _record@1),
                    [Css | erlang:element(3, Model)],
                    erlang:element(4, Model) - 1}
            end,
            {Model@1, lustre@effect:none()};

        {external_stylesheet_loaded, {error, _}} ->
            Model@2 = begin
                _record@2 = Model,
                {scene_model,
                    erlang:element(2, _record@2),
                    erlang:element(3, _record@2),
                    erlang:element(4, Model) - 1}
            end,
            {Model@2, lustre@effect:none()};

        {user_edited_value, Key@1, Value@1} ->
            {Model,
                lustre@event:emit(
                    <<"change"/utf8>>,
                    gleam@json:object(
                        [{<<"key"/utf8>>, gleam@json:int(Key@1)},
                            {<<"value"/utf8>>,
                                lustre_fable@value:to_json(Value@1)}]
                    )
                )}
    end.

-file("src/lustre_fable/story.gleam", 236).
?DOC(false).
-spec scene_view(
    scene_model(),
    fun((model()) -> lustre@vdom@vnode:element(msg()))
) -> lustre@vdom@vnode:element(msg()).
scene_view(Model, View) ->
    lustre@element:fragment(
        gleam@list:flatten(
            [gleam@list:map(
                    erlang:element(3, Model),
                    fun(Css) ->
                        lustre@element@html:style(
                            [],
                            gleam@string:replace(
                                Css,
                                <<":root"/utf8>>,
                                <<":host"/utf8>>
                            )
                        )
                    end
                ),
                [View({model, erlang:element(2, Model)}),
                    lustre@element@html:style(
                        [],
                        <<"
          :host {
            border-collapse: revert;
            border-spacing: revert;
            caption-side: revert;
            color: revert;
            cursor: revert;
            direction: revert;
            empty-cells: revert;
            font-family: revert;
            font-size: revert;
            font-style: revert;
            font-variant: revert;
            font-weight: revert;
            font-size-adjust: revert;
            font-stretch: revert;
            font: revert;
            letter-spacing: revert;
            line-height: revert;
            list-style-image: revert;
            list-style-position: revert;
            list-style-type: revert;
            list-style: revert;
            orphans: revert;
            quotes: revert;
            tab-size: revert;
            text-align: revert;
            text-align-last: revert;
            text-decoration-color: revert;
            text-indent: revert;
            text-justify: revert;
            text-shadow: revert;
            text-transform: revert;
            visibility: revert;
            white-space: revert;
            widows: revert;
            word-break: revert;
            word-spacing: revert;
            word-wrap: revert;
          }
          "/utf8>>
                    )]]
        )
    ).

-file("src/lustre_fable/story.gleam", 324).
?DOC(false).
-spec do_base_tag(binary(), integer()) -> binary().
do_base_tag(Base, Count) ->
    case lustre:is_registered(
        <<<<Base/binary, "-"/utf8>>/binary,
            (erlang:integer_to_binary(Count))/binary>>
    ) of
        true ->
            do_base_tag(Base, Count + 1);

        false ->
            <<<<Base/binary, "-"/utf8>>/binary,
                (erlang:integer_to_binary(Count))/binary>>
    end.

-file("src/lustre_fable/story.gleam", 311).
?DOC(false).
-spec base_tag(binary()) -> binary().
base_tag(Title) ->
    _assert_subject = gleam@regexp:from_string(<<"[^a-zA-Z0-9]+"/utf8>>),
    {ok, Re} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        value => _assert_fail,
                        module => <<"lustre_fable/story"/utf8>>,
                        function => <<"base_tag"/utf8>>,
                        line => 312})
    end,
    Safe_component_name = begin
        _pipe = Title,
        _pipe@1 = string:lowercase(_pipe),
        gleam_regexp_ffi:replace(Re, _pipe@1, <<"-"/utf8>>)
    end,
    case lustre:is_registered(Safe_component_name) of
        true ->
            do_base_tag(Safe_component_name, 1);

        false ->
            Safe_component_name
    end.

-file("src/lustre_fable/story.gleam", 71).
?DOC(false).
-spec register(story_config(), list(binary()), list(binary())) -> {ok, story()} |
    {error, lustre:error()}.
register(Config, Stylesheets, External_stylesheets) ->
    Base = base_tag(erlang:element(2, Config)),
    Component = <<Base/binary, "-story"/utf8>>,
    Scene = <<Base/binary, "-scene"/utf8>>,
    gleam@result:'try'(
        lustre:register(
            lustre:simple(
                fun story_init/1,
                fun story_update/2,
                (fun(_capture) ->
                    story_view(_capture, Scene, erlang:element(3, Config))
                end)
            ),
            Component
        ),
        fun(_) ->
            gleam@result:'try'(
                lustre:register(
                    lustre:component(
                        fun(_capture@1) ->
                            scene_init(
                                _capture@1,
                                Stylesheets,
                                External_stylesheets
                            )
                        end,
                        fun scene_update/2,
                        fun(_capture@2) ->
                            scene_view(_capture@2, erlang:element(5, Config))
                        end,
                        erlang:element(4, Config)
                    ),
                    Scene
                ),
                fun(_) ->
                    {ok,
                        {story,
                            erlang:element(2, Config),
                            Base,
                            Component,
                            Scene}}
                end
            )
        end
    ).
