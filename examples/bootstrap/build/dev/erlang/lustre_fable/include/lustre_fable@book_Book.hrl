-record(book, {
    title :: binary(),
    stylesheets :: list(binary()),
    external_stylesheets :: list(binary()),
    chapters :: list({binary(), list(lustre_fable@story:story_config())})
}).
