-record(event, {
    kind :: integer(),
    name :: binary(),
    handler :: gleam@dynamic@decode:decoder(any()),
    include :: list(binary()),
    prevent_default :: boolean(),
    stop_propagation :: boolean(),
    immediate :: boolean(),
    debounce :: integer(),
    throttle :: integer()
}).
