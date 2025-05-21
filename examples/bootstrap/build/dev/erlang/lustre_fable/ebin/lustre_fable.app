{application, lustre_fable, [
    {vsn, "1.0.0"},
    {applications, [gleam_json,
                    gleam_regexp,
                    gleam_stdlib,
                    justin,
                    lustre,
                    modem,
                    rsvp]},
    {description, ""},
    {modules, [lustre@dev@fable,
               lustre_fable@book,
               lustre_fable@ui]},
    {registered, []}
]}.
