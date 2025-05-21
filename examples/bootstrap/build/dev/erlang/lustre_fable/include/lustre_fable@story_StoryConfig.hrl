-record(story_config, {
    title :: binary(),
    inputs :: list(fun((lustre_fable@story:model()) -> lustre@vdom@vnode:element(lustre_fable@story:msg()))),
    options :: list(lustre@component:option(lustre_fable@story:msg())),
    view :: fun((lustre_fable@story:model()) -> lustre@vdom@vnode:element(lustre_fable@story:msg()))
}).
