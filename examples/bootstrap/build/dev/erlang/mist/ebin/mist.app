{application, mist, [
    {mod, {'mist@internal@clock', []}},
    {vsn, "4.0.7"},
    {applications, [gleam_erlang,
                    gleam_http,
                    gleam_otp,
                    gleam_stdlib,
                    gleam_yielder,
                    glisten,
                    gramps,
                    hpack,
                    logging]},
    {description, "a misty Gleam web server"},
    {modules, []},
    {registered, []}
]}.
