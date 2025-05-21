-record(events, {
    handlers :: lustre@internals@mutable_map:mutable_map(binary(), gleam@dynamic@decode:decoder(any())),
    dispatched_paths :: list(binary()),
    next_dispatched_paths :: list(binary())
}).
