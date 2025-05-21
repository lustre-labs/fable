-module(lustre@element@keyed).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([element/3, namespaced/4, fragment/1, ul/2, ol/2, 'div'/2, tbody/2, dl/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lustre uses something called a _virtual DOM_ to work out what has changed\n"
    " between renders and update the DOM accordingly. That means when you render\n"
    " items in a list, Lustre will walk through the list of items and compare them\n"
    " in order to see if they have changed.\n"
    "\n"
    " This is often fine but it can be cause problems in cases where we'd like\n"
    " Lustre to reuse existing DOM nodes more efficiently. Consider the example\n"
    " in the [quickstart guide](../../guide/01-quickstart.html): each time the\n"
    " counter is incremented, we insert a new image at the _start_ of the list.\n"
    "\n"
    " Let's see how the virtual DOM handles this:\n"
    "\n"
    " ```\n"
    " Increment ->                 Increment ->\n"
    "              <img src=\"a\">   -- update ->  <img src=\"b\">\n"
    "                              -- insert ->  <img src=\"a\">\n"
    " ```\n"
    "\n"
    " Beacuse the virtual DOM compares elements in order, it sees that the first\n"
    " element has its `src` attribute changed from `\"a\"` to `\"b\"` and then sees\n"
    " that a new element has been added to the _end_ of the list.\n"
    "\n"
    " Intuitively, we know that what _really_ happened is that an element was\n"
    " inserted at the _front_ of the list and ideally the first `<img />` should\n"
    " be left untouched.\n"
    "\n"
    " The solution is to assign a unique _key_ to each child element. This gives\n"
    " Lustre enough information to reuse existing DOM nodes and avoid unnecessary\n"
    " updates.\n"
    "\n"
    " Keyed elements in Lustre work exactly like regular elements, but their child\n"
    " list is a tuple of a unique key and the child itself:\n"
    "\n"
    " ```gleam\n"
    " keyed.div([], list.map(model.cats, fn(cat) {\n"
    "   #(cat.id, html.img([attribute.src(cat.url)]))\n"
    " }))\n"
    " ```\n"
    "\n"
    " Let's see how the virtual DOM now handles this:\n"
    "\n"
    " ```\n"
    " Increment ->                 Increment ->\n"
    "                              -- insert ->  <img src=\"b\">\n"
    "              <img href=\"a\">  --        ->  <img src=\"a\">\n"
    " ```\n"
    "\n"
    " We can see that Lustre has correctly recognised that the only change is a\n"
    " new image being inserted at the front of the list. The first image is left\n"
    " untouched!\n"
    "\n"
).

-file("src/lustre/element/keyed.gleam", 185).
-spec extract_keyed_children(list({binary(), lustre@vdom@vnode:element(UDV)})) -> {lustre@internals@mutable_map:mutable_map(binary(), lustre@vdom@vnode:element(UDV)),
    list(lustre@vdom@vnode:element(UDV)),
    integer()}.
extract_keyed_children(Children) ->
    Init = {gleam@dict:new(), [], 0},
    {Keyed_children@2, Children@2, Children_count@1} = begin
        gleam@list:fold(
            Children,
            Init,
            fun(_use0, _use1) ->
                {Keyed_children, Children@1, Children_count} = _use0,
                {Key, Element} = _use1,
                Keyed_element = lustre@vdom@vnode:to_keyed(Key, Element),
                Keyed_children@1 = case Key of
                    <<""/utf8>> ->
                        Keyed_children;

                    _ ->
                        gleam@dict:insert(Keyed_children, Key, Keyed_element)
                end,
                {Keyed_children@1,
                    [Keyed_element | Children@1],
                    Children_count + 1}
            end
        )
    end,
    {Keyed_children@2, lists:reverse(Children@2), Children_count@1}.

-file("src/lustre/element/keyed.gleam", 74).
?DOC(
    " Render a _keyed_ element with the given tag. Each child is assigned a unique\n"
    " key, which Lustre uses to identify the element in the DOM. This is useful when\n"
    " a single child can be moved around such as in a to-do list, or when elements\n"
    " are frequently added or removed.\n"
    "\n"
    " > **Note**: the key for each child must be unique within the list of children,\n"
    " > but it doesn't have to be unique across the whole application. It's fine to\n"
    " > use the same key in different lists.\n"
).
-spec element(
    binary(),
    list(lustre@vdom@vattr:attribute(UCB)),
    list({binary(), lustre@vdom@vnode:element(UCB)})
) -> lustre@vdom@vnode:element(UCB).
element(Tag, Attributes, Children) ->
    {Keyed_children, Children@1, _} = extract_keyed_children(Children),
    lustre@vdom@vnode:element(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        <<""/utf8>>,
        Tag,
        Attributes,
        Children@1,
        Keyed_children,
        false,
        false
    ).

-file("src/lustre/element/keyed.gleam", 103).
?DOC(
    " Render a _keyed_ element with the given namespace and tag. Each child is\n"
    " assigned a unique key, which Lustre uses to identify the element in the DOM.\n"
    " This is useful when a single child can be moved around such as in a to-do\n"
    " list, or when elements are frequently added or removed.\n"
    "\n"
    " > **Note**: the key for each child must be unique within the list of children,\n"
    " > but it doesn't have to be unique across the whole application. It's fine to\n"
    " > use the same key in different lists.\n"
).
-spec namespaced(
    binary(),
    binary(),
    list(lustre@vdom@vattr:attribute(UCH)),
    list({binary(), lustre@vdom@vnode:element(UCH)})
) -> lustre@vdom@vnode:element(UCH).
namespaced(Namespace, Tag, Attributes, Children) ->
    {Keyed_children, Children@1, _} = extract_keyed_children(Children),
    lustre@vdom@vnode:element(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Namespace,
        Tag,
        Attributes,
        Children@1,
        Keyed_children,
        false,
        false
    ).

-file("src/lustre/element/keyed.gleam", 133).
?DOC(
    " Render a _keyed_ fragment. Each child is assigned a unique key, which Lustre\n"
    " uses to identify the element in the DOM. This is useful when a single child\n"
    " can be moved around such as in a to-do list, or when elements are frequently\n"
    " added or removed.\n"
    "\n"
    " > **Note**: the key for each child must be unique within the list of children,\n"
    " > but it doesn't have to be unique across the whole application. It's fine to\n"
    " > use the same key in different lists.\n"
).
-spec fragment(list({binary(), lustre@vdom@vnode:element(UCN)})) -> lustre@vdom@vnode:element(UCN).
fragment(Children) ->
    {Keyed_children, Children@1, Children_count} = extract_keyed_children(
        Children
    ),
    lustre@vdom@vnode:fragment(
        <<""/utf8>>,
        fun gleam@function:identity/1,
        Children@1,
        Keyed_children,
        Children_count
    ).

-file("src/lustre/element/keyed.gleam", 148).
-spec ul(
    list(lustre@vdom@vattr:attribute(UCR)),
    list({binary(), lustre@vdom@vnode:element(UCR)})
) -> lustre@vdom@vnode:element(UCR).
ul(Attributes, Children) ->
    element(<<"ul"/utf8>>, Attributes, Children).

-file("src/lustre/element/keyed.gleam", 155).
-spec ol(
    list(lustre@vdom@vattr:attribute(UCX)),
    list({binary(), lustre@vdom@vnode:element(UCX)})
) -> lustre@vdom@vnode:element(UCX).
ol(Attributes, Children) ->
    element(<<"ol"/utf8>>, Attributes, Children).

-file("src/lustre/element/keyed.gleam", 162).
-spec 'div'(
    list(lustre@vdom@vattr:attribute(UDD)),
    list({binary(), lustre@vdom@vnode:element(UDD)})
) -> lustre@vdom@vnode:element(UDD).
'div'(Attributes, Children) ->
    element(<<"div"/utf8>>, Attributes, Children).

-file("src/lustre/element/keyed.gleam", 169).
-spec tbody(
    list(lustre@vdom@vattr:attribute(UDJ)),
    list({binary(), lustre@vdom@vnode:element(UDJ)})
) -> lustre@vdom@vnode:element(UDJ).
tbody(Attributes, Children) ->
    element(<<"tbody"/utf8>>, Attributes, Children).

-file("src/lustre/element/keyed.gleam", 176).
-spec dl(
    list(lustre@vdom@vattr:attribute(UDP)),
    list({binary(), lustre@vdom@vnode:element(UDP)})
) -> lustre@vdom@vnode:element(UDP).
dl(Attributes, Children) ->
    element(<<"dl"/utf8>>, Attributes, Children).
