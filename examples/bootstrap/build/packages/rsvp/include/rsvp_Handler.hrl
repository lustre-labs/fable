-record(handler, {
    run :: fun(({ok, gleam@http@response:response(binary())} |
        {error, rsvp:error()}) -> any())
}).
