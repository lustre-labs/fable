-module(lustre@component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/1, on_attribute_change/2, on_property_change/2, form_associated/0, on_form_autofill/1, on_form_reset/1, on_form_restore/1, open_shadow_root/1, adopt_styles/1, to_server_component_config/1, default_slot/2, named_slot/3, part/1, exportparts/1, slot/1, set_form_value/1, clear_form_value/0, set_pseudo_state/1, remove_pseudo_state/1]).
-export_type([config/1, option/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lustre's component system is built on top of the Custom Elements API and\n"
    " the Shadow DOM API. This module helps you configure new components and\n"
    " interact with existing ones.\n"
    "\n"
    " While it's not required, understanding the spec and how it works will help\n"
    " you get the most out of Lustre's component system. The following resources\n"
    " are a great place to start:\n"
    "\n"
    "   - https://developer.mozilla.org/en-US/docs/Web/Web_Components\n"
    "\n"
    "   - https://css-tricks.com/web-components-demystified/\n"
    "\n"
    "   - https://github.com/web-padawan/awesome-web-components\n"
    "\n"
    " ## Examples\n"
    "\n"
    " We have a small number of examples showing how to set up and use components\n"
    " that are a great place to see some code:\n"
    "\n"
    " - [`Basic setup`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/01-basic-setup)\n"
    "\n"
    " - [`Custom attributes and events`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/02-attributes-and-events)\n"
    "\n"
    " - [`Slots`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/03-slots)\n"
    "\n"
    " This list of examples is likely to grow over time, so be sure to check back\n"
    " every now and then to see what's new!\n"
    "\n"
    " ## Getting help\n"
    "\n"
    " If you're having trouble with Lustre or not sure what the right way to do\n"
    " something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    " You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).\n"
    "\n"
).

-opaque config(SQW) :: {config,
        boolean(),
        boolean(),
        gleam@dict:dict(binary(), fun((binary()) -> {ok, SQW} | {error, nil})),
        gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(SQW)),
        boolean(),
        gleam@option:option(fun((binary()) -> SQW)),
        gleam@option:option(SQW),
        gleam@option:option(fun((binary()) -> SQW))}.

-opaque option(SQX) :: {option, fun((config(SQX)) -> config(SQX))}.

-file("src/lustre/component.gleam", 131).
?DOC(false).
-spec new(list(option(SQY))) -> config(SQY).
new(Options) ->
    Init = {config,
        false,
        true,
        gleam@dict:new(),
        gleam@dict:new(),
        false,
        none,
        none,
        none},
    gleam@list:fold(
        Options,
        Init,
        fun(Config, Option) -> (erlang:element(2, Option))(Config) end
    ).

-file("src/lustre/component.gleam", 163).
?DOC(
    " Register a decoder to run whenever the named attribute changes. Attributes\n"
    " can be set in Lustre using the [`attribute`](./attribute.html#attribute)\n"
    " function, set directly on the component's HTML tag, or in JavaScript using\n"
    " the [`setAttribute`](https://developer.mozilla.org/en-US/docs/Web/API/Element/setAttribute)\n"
    " method.\n"
    "\n"
    " Attributes are always strings, but your decoder is responsible for decoding\n"
    " the string into a message that your component can understand.\n"
).
-spec on_attribute_change(binary(), fun((binary()) -> {ok, SRC} | {error, nil})) -> option(SRC).
on_attribute_change(Name, Decoder) ->
    {option,
        fun(Config) ->
            Attributes = gleam@dict:insert(
                erlang:element(4, Config),
                Name,
                Decoder
            ),
            _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                Attributes,
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)}
        end}.

-file("src/lustre/component.gleam", 181).
?DOC(
    " Register decoder to run whenever the given property is set on the component.\n"
    " Properties can be set in Lustre using the [`property`](./attribute.html#property)\n"
    " function or in JavaScript by setting a property directly on the component\n"
    " object.\n"
    "\n"
    " Properties can be any JavaScript object. For server components, properties\n"
    " will be any _JSON-serialisable_ value.\n"
).
-spec on_property_change(binary(), gleam@dynamic@decode:decoder(SRG)) -> option(SRG).
on_property_change(Name, Decoder) ->
    {option,
        fun(Config) ->
            Properties = gleam@dict:insert(
                erlang:element(5, Config),
                Name,
                Decoder
            ),
            _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                Properties,
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)}
        end}.

-file("src/lustre/component.gleam", 196).
?DOC(
    " Mark a component as \"form-associated\". This lets your component participate\n"
    " in form submission and respond to additional form-specific events such as\n"
    " the form being reset or the browser autofilling this component's value.\n"
    "\n"
    " > **Note**: form-associated components are not supported in server components\n"
    " > for both technical and ideological reasons. If you'd like a component that\n"
    " > participates in form submission, you should use a client component!\n"
).
-spec form_associated() -> option(any()).
form_associated() ->
    {option, fun(Config) -> _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                true,
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)} end}.

-file("src/lustre/component.gleam", 210).
?DOC(
    " Register a callback that runs when the browser autofills this\n"
    " [form-associated](#form_associated) component's `\"value\"` attribute. The\n"
    " callback should convert the autofilled value into a message that you handle\n"
    " in your `update` function.\n"
    "\n"
    " > **Note**: server components cannot participate in form submission and configuring\n"
    " > this option will do nothing.\n"
).
-spec on_form_autofill(fun((binary()) -> SRL)) -> option(SRL).
on_form_autofill(Handler) ->
    {option, fun(Config) -> _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                true,
                {some, Handler},
                erlang:element(8, _record),
                erlang:element(9, _record)} end}.

-file("src/lustre/component.gleam", 222).
?DOC(
    " Set a message to be dispatched whenever a form containing this\n"
    " [form-associated](#form_associated) component is reset.\n"
    "\n"
    " > **Note**: server components cannot participate in form submission and configuring\n"
    " > this option will do nothing.\n"
).
-spec on_form_reset(SRN) -> option(SRN).
on_form_reset(Msg) ->
    {option, fun(Config) -> _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                true,
                erlang:element(7, _record),
                {some, Msg},
                erlang:element(9, _record)} end}.

-file("src/lustre/component.gleam", 235).
?DOC(
    " Set a callback that runs when the browser restores this\n"
    " [form-associated](#form_associated) component's `\"value\"` attribute. This is\n"
    " often triggered when the user navigates back or forward in their history.\n"
    "\n"
    " > **Note**: server components cannot participate in form submission and configuring\n"
    " > this option will do nothing.\n"
).
-spec on_form_restore(fun((binary()) -> SRP)) -> option(SRP).
on_form_restore(Handler) ->
    {option, fun(Config) -> _record = Config,
            {config,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                true,
                erlang:element(7, _record),
                erlang:element(8, _record),
                {some, Handler}} end}.

-file("src/lustre/component.gleam", 249).
?DOC(
    " Configure whether a component's [Shadow Root](https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot)\n"
    " is open or closed. A closed shadow root means the elements rendered inside\n"
    " the component are not accessible from JavaScript outside the component.\n"
    "\n"
    " By default a component's shadow root is **open**. You may want to configure\n"
    " this option manually if you intend to build a component for use outside of\n"
    " Lustre.\n"
).
-spec open_shadow_root(boolean()) -> option(any()).
open_shadow_root(Open) ->
    {option, fun(Config) -> _record = Config,
            {config,
                Open,
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)} end}.

-file("src/lustre/component.gleam", 269).
?DOC(
    " Configure whether a component should attempt to adopt stylesheets from\n"
    " its parent document. Components in Lustre use the shadow DOM to unlock native\n"
    " web component features like slots, but this means elements rendered inside a\n"
    " component are isolated from the document's styles.\n"
    "\n"
    " To get around this, Lustre can attempt to adopt all stylesheets from the\n"
    " parent document when the component is first created; meaning in many cases\n"
    " you can use the same CSS to style your components as you do the rest of your\n"
    " application.\n"
    "\n"
    " By default, this option is **enabled**. You may want to disable this option\n"
    " if you are building a component for use outside of Lustre and do not want\n"
    " document styles to interfere with your component's styling\n"
).
-spec adopt_styles(boolean()) -> option(any()).
adopt_styles(Adopt) ->
    {option, fun(Config) -> _record = Config,
            {config,
                erlang:element(2, _record),
                Adopt,
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record)} end}.

-file("src/lustre/component.gleam", 288).
?DOC(false).
-spec to_server_component_config(config(SRV)) -> lustre@runtime@server@runtime:config(SRV).
to_server_component_config(Config) ->
    {config,
        erlang:element(2, Config),
        erlang:element(3, Config),
        erlang:element(4, Config),
        erlang:element(5, Config)}.

-file("src/lustre/component.gleam", 311).
?DOC(
    " Create a default slot for a component. Any elements rendered as children of\n"
    " the component will be placed inside the default slot unless explicitly\n"
    " redirected using the [`slot`](#slot) attribute.\n"
    "\n"
    " If no children are placed into the slot, the `fallback` elements will be\n"
    " rendered instead.\n"
    "\n"
    " To learn more about Shadow DOM and slots, see this excellent guide:\n"
    "\n"
    "   https://javascript.info/slots-composition\n"
).
-spec default_slot(
    list(lustre@vdom@vattr:attribute(SRY)),
    list(lustre@vdom@vnode:element(SRY))
) -> lustre@vdom@vnode:element(SRY).
default_slot(Attributes, Fallback) ->
    lustre@element@html:slot(Attributes, Fallback).

-file("src/lustre/component.gleam", 329).
?DOC(
    " Create a named slot for a component. Any elements rendered as children of\n"
    " the component with a [`slot`](#slot) attribute matching the `name` will be\n"
    " rendered inside this slot.\n"
    "\n"
    " If no children are placed into the slot, the `fallback` elements will be\n"
    " rendered instead.\n"
    "\n"
    " To learn more about Shadow DOM and slots, see this excellent guide:\n"
    "\n"
    "   https://javascript.info/slots-composition\n"
).
-spec named_slot(
    binary(),
    list(lustre@vdom@vattr:attribute(SSE)),
    list(lustre@vdom@vnode:element(SSE))
) -> lustre@vdom@vnode:element(SSE).
named_slot(Name, Attributes, Fallback) ->
    lustre@element@html:slot(
        [lustre@attribute:attribute(<<"name"/utf8>>, Name) | Attributes],
        Fallback
    ).

-file("src/lustre/component.gleam", 380).
?DOC(
    " Lustre's component system is built on top the Custom Elements API and the\n"
    " Shadow DOM API. A component's `view` function is rendered inside a shadow\n"
    " root, which means the component's HTML is isolated from the rest of the\n"
    " document.\n"
    "\n"
    " This can make it difficult to style components from CSS outside the component.\n"
    " To help with this, the `part` attribute lets you expose parts of your component\n"
    " by name to be styled by external CSS.\n"
    "\n"
    " For example, if the `view` function for a component called `\"my-component`\"\n"
    " looks like this:\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    " import lustre/component\n"
    " import lustre/element/html\n"
    "\n"
    " fn view(model) {\n"
    "   html.div([], [\n"
    "     html.button([], [html.text(\"-\")]),\n"
    "     html.p([component.part(\"count\")], [html.text(int.to_string(model.count))]),\n"
    "     html.button([], [html.text(\"+\")]),\n"
    "   ])\n"
    " }\n"
    " ```\n"
    "\n"
    " Then the following CSS in the **parent** document can be used to style the\n"
    " `<p>` element:\n"
    "\n"
    " ```css\n"
    " my-component::part(count) {\n"
    "   color: red;\n"
    " }\n"
    " ```\n"
    "\n"
    " To learn more about the CSS Shadow Parts specification, see:\n"
    "\n"
    "   - https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/part\n"
    "\n"
    "   - https://developer.mozilla.org/en-US/docs/Web/CSS/::part\n"
).
-spec part(binary()) -> lustre@vdom@vattr:attribute(any()).
part(Name) ->
    lustre@attribute:attribute(<<"part"/utf8>>, Name).

-file("src/lustre/component.gleam", 441).
?DOC(
    " While the [`part`](#part) attribute can be used to expose parts of a component\n"
    " to its parent, these parts will not automatically become available to the\n"
    " _document_ when components are nested inside each other.\n"
    "\n"
    " The `exportparts` attribute lets you forward the parts of a nested component\n"
    " to the parent component so they can be styled from the parent document.\n"
    "\n"
    " Consider we have two components, `\"my-component\"` and `\"my-nested-component\"`\n"
    " with the following `view` functions:\n"
    "\n"
    " ```gleam\n"
    " import gleam/int\n"
    " import lustre/attribute.{property}\n"
    " import lustre/component\n"
    " import lustre/element.{element}\n"
    " import lustre/element/html\n"
    "\n"
    " fn my_component_view(model) {\n"
    "   html.div([], [\n"
    "     html.button([], [html.text(\"-\")]),\n"
    "     element(\n"
    "       \"my-nested-component\",\n"
    "       [\n"
    "         property(\"count\", model.count),\n"
    "         component.exportparts([\"count\"]),\n"
    "       ],\n"
    "       []\n"
    "     )\n"
    "     html.button([], [html.text(\"+\")]),\n"
    "   ])\n"
    " }\n"
    "\n"
    " fn my_nested_component_view(model) {\n"
    "   html.p([component.part(\"count\")], [html.text(int.to_string(model.count))])\n"
    " }\n"
    " ```\n"
    "\n"
    " The `<my-nested-component />` component has a part called `\"count\"` which the\n"
    " `<my-component />` then forwards to the parent document using the `\"exportparts\"`\n"
    " attribute. Now the following CSS can be used to style the `<p>` element nested\n"
    " deep inside the `<my-component />`:\n"
    "\n"
    " ```css\n"
    " my-component::part(count) {\n"
    "   color: red;\n"
    " }\n"
    " ```\n"
    "\n"
    " Notice how the styles are applied to the `<my-component />` element, not the\n"
    " `<my-nested-component />` element!\n"
    "\n"
    " To learn more about the CSS Shadow Parts specification, see:\n"
    "\n"
    "   - https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/exportparts\n"
    "\n"
    "   - https://developer.mozilla.org/en-US/docs/Web/CSS/::part\n"
).
-spec exportparts(list(binary())) -> lustre@vdom@vattr:attribute(any()).
exportparts(Names) ->
    lustre@attribute:attribute(
        <<"exportparts"/utf8>>,
        gleam@string:join(Names, <<", "/utf8>>)
    ).

-file("src/lustre/component.gleam", 454).
?DOC(
    " Associate an element with a [named slot](#named_slot) in a component. Multiple\n"
    " elements can be associated with the same slot name.\n"
    "\n"
    " To learn more about Shadow DOM and slots, see:\n"
    "\n"
    "   https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes/slot\n"
    "\n"
    "   https://javascript.info/slots-composition\n"
).
-spec slot(binary()) -> lustre@vdom@vattr:attribute(any()).
slot(Name) ->
    lustre@attribute:attribute(<<"slot"/utf8>>, Name).

-file("src/lustre/component.gleam", 471).
-spec do_set_form_value(gleam@dynamic:dynamic_(), binary()) -> nil.
do_set_form_value(_, _) ->
    nil.

-file("src/lustre/component.gleam", 465).
?DOC(
    " Set the value of a [form-associated component](#form_associated). If the\n"
    " component is rendered inside a `<form>` element, the value will be\n"
    " automatically included in the form submission and available in the form's\n"
    " `FormData` object.\n"
).
-spec set_form_value(binary()) -> lustre@effect:effect(any()).
set_form_value(Value) ->
    lustre@effect:before_paint(
        fun(_, Root) -> do_set_form_value(Root, Value) end
    ).

-file("src/lustre/component.gleam", 485).
-spec do_clear_form_value(gleam@dynamic:dynamic_()) -> nil.
do_clear_form_value(_) ->
    nil.

-file("src/lustre/component.gleam", 479).
?DOC(
    " Clear a form value previously set with [`set_form_value`](#set_form_value).\n"
    " When the form is submitted, this component's value will not be included in\n"
    " the form data.\n"
).
-spec clear_form_value() -> lustre@effect:effect(any()).
clear_form_value() ->
    lustre@effect:before_paint(fun(_, Root) -> do_clear_form_value(Root) end).

-file("src/lustre/component.gleam", 515).
-spec do_set_pseudo_state(gleam@dynamic:dynamic_(), binary()) -> nil.
do_set_pseudo_state(_, _) ->
    nil.

-file("src/lustre/component.gleam", 509).
?DOC(
    " Set a custom state on the component. This state is not reflected in the DOM\n"
    " but can be selected in CSS using the `:state` pseudo-class. For example,\n"
    " calling `set_pseudo_state(\"checked\")` on a component called `\"my-checkbox\"`\n"
    " means the following CSS will apply:\n"
    "\n"
    " ```css\n"
    " my-checkbox:state(checked) {\n"
    "   border: solid;\n"
    " }\n"
    " ```\n"
    "\n"
    " If you are styling a component by rendering a `<style>` element _inside_ the\n"
    " component, the previous CSS would be rewritten as:\n"
    "\n"
    " ```css\n"
    " :host(:state(checked)) {\n"
    "   border: solid;\n"
    " }\n"
    " ```\n"
).
-spec set_pseudo_state(binary()) -> lustre@effect:effect(any()).
set_pseudo_state(Value) ->
    lustre@effect:before_paint(
        fun(_, Root) -> do_set_pseudo_state(Root, Value) end
    ).

-file("src/lustre/component.gleam", 527).
-spec do_remove_pseudo_state(gleam@dynamic:dynamic_(), binary()) -> nil.
do_remove_pseudo_state(_, _) ->
    nil.

-file("src/lustre/component.gleam", 521).
?DOC(" Remove a custom state set by [`set_pseudo_state`](#set_pseudo_state).\n").
-spec remove_pseudo_state(binary()) -> lustre@effect:effect(any()).
remove_pseudo_state(Value) ->
    lustre@effect:before_paint(
        fun(_, Root) -> do_remove_pseudo_state(Root, Value) end
    ).
