-record(config, {
    open_shadow_root :: boolean(),
    adopt_styles :: boolean(),
    attributes :: gleam@dict:dict(binary(), fun((binary()) -> {ok, any()} |
        {error, nil})),
    properties :: gleam@dict:dict(binary(), gleam@dynamic@decode:decoder(any())),
    is_form_associated :: boolean(),
    on_form_autofill :: gleam@option:option(fun((binary()) -> any())),
    on_form_reset :: gleam@option:option(any()),
    on_form_restore :: gleam@option:option(fun((binary()) -> any()))
}).
