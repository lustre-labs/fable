-module(lustre@element).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([element/3, namespaced/4, advanced/6, text/1, none/0, fragment/1, unsafe_raw_html/4, map/2, to_string/1, to_document_string/1, to_string_tree/1, to_document_string_tree/1, to_readable_string/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lustre wouldn't be much use as a frontend framework if it didn't provide a\n"
    " way to create HTML elements. This module contains the basic functions\n"
    " necessary to construct and manipulate different HTML elements.\n"
    "\n"
    " It is also possible to use Lustre as a HTML templating library, without\n"
    " using its runtime or framework features.\n"
    "\n"
).

-file("src/lustre/element.gleam", 100).
?DOC(
    " A general function for constructing any kind of element. In most cases you\n"
    " will want to use the [`lustre/element/html`](./element/html.html) instead but this\n"
    " function is particularly handy when constructing custom elements, either\n"
    " from your own Lustre components or from external JavaScript libraries.\n"
    "\n"
    " > **Note**: Because Lustre is primarily used to create HTML, this function\n"
    " > special-cases the following tags which render as\n"
    " > [void elements](https://developer.mozilla.org/en-US/docs/Glossary/Void_element):\n"
    " >\n"
    " >   - area\n"
    " >   - base\n"
    " >   - br\n"
    " >   - col\n"
    " >   - embed\n"
    " >   - hr\n"
    " >   - img\n"
    " >   - input\n"
    " >   - link\n"
    " >   - meta\n"
    " >   - param\n"
    " >   - source\n"
    " >   - track\n"
    " >   - wbr\n"
    " >\n"
    " > This will only affect the output of `to_string` and `to_string_builder`!\n"
    " > If you need to render any of these tags with children, *or* you want to\n"
    " > render some other tag as self-closing or void, use [`advanced`](#advanced)\n"
    " > to construct the element instead.\n"
).
-spec element(
    binary(),
    list(lustre@vdom@vattr:attribute(REP)),
    list(lustre@vdom@vnode:element(REP))
) -> lustre@vdom@vnode:element(REP).
element(Tag, Attributes, Children) ->
    lustre@vdom@vnode:element(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        <<""/utf8>>,
        Tag,
        Attributes,
        Children,
        gleam@dict:new(),
        false,
        false
    ).

-file("src/lustre/element.gleam", 121).
?DOC(
    " A function for constructing elements in a specific XML namespace. This can\n"
    " be used to construct SVG or MathML elements, for example.\n"
).
-spec namespaced(
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(REV)),
    list(lustre@vdom@vnode:element(REV))
) -> lustre@vdom@vnode:element(REV).
namespaced(Namespace, Tag, Attributes, Children) ->
    lustre@vdom@vnode:element(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Namespace,
        Tag,
        Attributes,
        Children,
        gleam@dict:new(),
        false,
        false
    ).

-file("src/lustre/element.gleam", 145).
?DOC(
    " A function for constructing elements with more control over how the element\n"
    " is rendered when converted to a string. This is necessary because some HTML,\n"
    " SVG, and MathML elements are self-closing or void elements, and Lustre needs\n"
    " to know how to render them correctly!\n"
).
-spec advanced(
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(RFB)),
    list(lustre@vdom@vnode:element(RFB)),
    boolean(),
    boolean()
) -> lustre@vdom@vnode:element(RFB).
advanced(Namespace, Tag, Attributes, Children, Self_closing, Void) ->
    lustre@vdom@vnode:element(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Namespace,
        Tag,
        Attributes,
        Children,
        gleam@dict:new(),
        Self_closing,
        Void
    ).

-file("src/lustre/element.gleam", 171).
?DOC(
    " A function for turning a Gleam string into a text node. Gleam doesn't have\n"
    " union types like some other languages you may be familiar with, like TypeScript.\n"
    " Instead, we need a way to take a `String` and turn it into an `Element` somehow:\n"
    " this function is exactly that!\n"
).
-spec text(binary()) -> lustre@vdom@vnode:element(any()).
text(Content) ->
    lustre@vdom@vnode:text(<<""/utf8>>, fun gleam@function:identity/1, Content).

-file("src/lustre/element.gleam", 179).
?DOC(
    " A function for rendering nothing. This is mostly useful for conditional\n"
    " rendering, where you might want to render something only if a certain\n"
    " condition is met.\n"
).
-spec none() -> lustre@vdom@vnode:element(any()).
none() ->
    lustre@vdom@vnode:text(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        <<""/utf8>>
    ).

-file("src/lustre/element.gleam", 198).
-spec count_fragment_children(list(lustre@vdom@vnode:element(any())), integer()) -> integer().
count_fragment_children(Children, Count) ->
    case Children of
        [] ->
            Count;

        [{fragment, _, _, _, _, _, Children_count} | Rest] ->
            count_fragment_children(Rest, Count + Children_count);

        [_ | Rest@1] ->
            count_fragment_children(Rest@1, Count + 1)
    end.

-file("src/lustre/element.gleam", 188).
?DOC(
    " A function for constructing a wrapper element with no tag name. This is\n"
    " useful for wrapping a list of elements together without adding an extra\n"
    " `<div>` or other container element, or returning multiple elements in places\n"
    " where only one `Element` is expected.\n"
).
-spec fragment(list(lustre@vdom@vnode:element(RFL))) -> lustre@vdom@vnode:element(RFL).
fragment(Children) ->
    lustre@vdom@vnode:fragment(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Children,
        gleam@dict:new(),
        count_fragment_children(Children, 0)
    ).

-file("src/lustre/element.gleam", 218).
?DOC(
    " A function for constructing a wrapper element with custom raw HTML as its\n"
    " content. Lustre will render the provided HTML verbatim, and will not touch\n"
    " its children except when replacing the entire inner html on changes.\n"
    "\n"
    " > **Note:** The provided HTML will not be escaped automatically and may expose\n"
    " > your applications to XSS attacks! Make sure you absolutely trust the HTML you\n"
    " > pass to this function. In particular, never use this to display un-sanitised\n"
    " > user HTML!\n"
).
-spec unsafe_raw_html(
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(RFS)),
    binary()
) -> lustre@vdom@vnode:element(RFS).
unsafe_raw_html(Namespace, Tag, Attributes, Inner_html) ->
    lustre@vdom@vnode:unsafe_inner_html(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Namespace,
        Tag,
        Attributes,
        Inner_html
    ).

-file("src/lustre/element.gleam", 243).
?DOC(
    " The `Element` type is parameterised by the type of messages it can produce\n"
    " from events. Sometimes you might end up with a fragment of HTML from another\n"
    " library or module that produces a different type of message: this function lets\n"
    " you map the messages produced from one type to another.\n"
    "\n"
    " Think of it like `list.map` or `result.map` but for HTML events!\n"
).
-spec map(lustre@vdom@vnode:element(RFW), fun((RFW) -> RFY)) -> lustre@vdom@vnode:element(RFY).
map(Element, F) ->
    Mapper = gleam@function:identity(
        lustre@vdom@events:compose_mapper(
            gleam@function:identity(F),
            erlang:element(4, Element)
        )
    ),
    case Element of
        {fragment, _, _, _, Children, Keyed_children, _} ->
            _record = Element,
            {fragment,
                erlang:element(2, _record),
                erlang:element(3, _record),
                Mapper,
                gleam@function:identity(Children),
                gleam@function:identity(Keyed_children),
                erlang:element(7, _record)};

        {element, _, _, _, _, _, Attributes, Children@1, Keyed_children@1, _, _} ->
            _record@1 = Element,
            {element,
                erlang:element(2, _record@1),
                erlang:element(3, _record@1),
                Mapper,
                erlang:element(5, _record@1),
                erlang:element(6, _record@1),
                gleam@function:identity(Attributes),
                gleam@function:identity(Children@1),
                gleam@function:identity(Keyed_children@1),
                erlang:element(10, _record@1),
                erlang:element(11, _record@1)};

        {unsafe_inner_html, _, _, _, _, _, Attributes@1, _} ->
            _record@2 = Element,
            {unsafe_inner_html,
                erlang:element(2, _record@2),
                erlang:element(3, _record@2),
                Mapper,
                erlang:element(5, _record@2),
                erlang:element(6, _record@2),
                gleam@function:identity(Attributes@1),
                erlang:element(8, _record@2)};

        {text, _, _, _, _} ->
            gleam@function:identity(Element)
    end.

-file("src/lustre/element.gleam", 283).
?DOC(
    " Convert a Lustre `Element` to a string. This is _not_ pretty-printed, so\n"
    " there are no newlines or indentation. If you need to pretty-print an element,\n"
    " reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or\n"
    " [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your\n"
    " use case and we'll see what we can do!\n"
).
-spec to_string(lustre@vdom@vnode:element(any())) -> binary().
to_string(Element) ->
    lustre@vdom@vnode:to_string(Element).

-file("src/lustre/element.gleam", 294).
?DOC(
    " Converts an element to a string like [`to_string`](#to_string), but prepends\n"
    " a `<!doctype html>` declaration to the string. This is useful for rendering\n"
    " complete HTML documents.\n"
    "\n"
    " If the provided element is not an `html` element, it will be wrapped in both\n"
    " a `html` and `body` element.\n"
).
-spec to_document_string(lustre@vdom@vnode:element(any())) -> binary().
to_document_string(El) ->
    _pipe = lustre@vdom@vnode:to_string(case El of
            {element, _, _, _, _, <<"html"/utf8>>, _, _, _, _, _} ->
                El;

            {element, _, _, _, _, <<"head"/utf8>>, _, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, _, _, <<"body"/utf8>>, _, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string:append(<<"<!doctype html>\n"/utf8>>, _pipe).

-file("src/lustre/element.gleam", 310).
?DOC(
    " Convert a Lustre `Element` to a `StringTree`. This is _not_ pretty-printed,\n"
    " so there are no newlines or indentation. If you need to pretty-print an element,\n"
    " reach out on the [Gleam Discord](https://discord.gg/Fm8Pwmy) or\n"
    " [open an issue](https://github.com/lustre-labs/lustre/issues/new) with your\n"
    " use case and we'll see what we can do!\n"
).
-spec to_string_tree(lustre@vdom@vnode:element(any())) -> gleam@string_tree:string_tree().
to_string_tree(Element) ->
    lustre@vdom@vnode:to_string_tree(Element).

-file("src/lustre/element.gleam", 321).
?DOC(
    " Converts an element to a `StringTree` like [`to_string_builder`](#to_string_builder),\n"
    " but prepends a `<!doctype html>` declaration. This is useful for rendering\n"
    " complete HTML documents.\n"
    "\n"
    " If the provided element is not an `html` element, it will be wrapped in both\n"
    " a `html` and `body` element.\n"
).
-spec to_document_string_tree(lustre@vdom@vnode:element(any())) -> gleam@string_tree:string_tree().
to_document_string_tree(El) ->
    _pipe = lustre@vdom@vnode:to_string_tree(case El of
            {element, _, _, _, _, <<"html"/utf8>>, _, _, _, _, _} ->
                El;

            {element, _, _, _, _, <<"head"/utf8>>, _, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            {element, _, _, _, _, <<"body"/utf8>>, _, _, _, _, _} ->
                element(<<"html"/utf8>>, [], [El]);

            _ ->
                element(
                    <<"html"/utf8>>,
                    [],
                    [element(<<"body"/utf8>>, [], [El])]
                )
        end),
    gleam@string_tree:prepend(_pipe, <<"<!doctype html>\n"/utf8>>).

-file("src/lustre/element.gleam", 355).
?DOC(
    " Converts a Lustre `Element` to a human-readable string by inserting new lines\n"
    " and indentation where appropriate. This is useful for debugging and testing,\n"
    " but for production code you should use [`to_string`](#to_string) or\n"
    " [`to_document_string`](#to_document_string) instead.\n"
    "\n"
    " 💡 This function works great with the snapshot testing library\n"
    "    [birdie](https://hexdocs.pm/birdie)!\n"
    "\n"
    " ## Using `to_string`:\n"
    "\n"
    " ```html\n"
    " <header><h1>Hello, world!</h1></header>\n"
    " ```\n"
    "\n"
    " ## Using `to_readable_string`\n"
    "\n"
    " ```html\n"
    " <header>\n"
    "   <h1>\n"
    "     Hello, world!\n"
    "   </h1>\n"
    " </header>\n"
    " ```\n"
).
-spec to_readable_string(lustre@vdom@vnode:element(any())) -> binary().
to_readable_string(El) ->
    lustre@vdom@vnode:to_snapshot(El).
