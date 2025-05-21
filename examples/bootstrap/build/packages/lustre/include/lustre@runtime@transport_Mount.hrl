-record(mount, {
    kind :: integer(),
    open_shadow_root :: boolean(),
    will_adopt_styles :: boolean(),
    observed_attributes :: list(binary()),
    observed_properties :: list(binary()),
    vdom :: lustre@vdom@vnode:element(any())
}).
