-module(lustre_dev_tools@error).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([invalid_proxy_target/1, explain/1]).
-export_type([error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-type error() :: {build_error, binary()} |
    {bundle_error, binary()} |
    {cannot_create_directory, simplifile:file_error(), binary()} |
    {cannot_read_file, simplifile:file_error(), binary()} |
    {cannot_set_permissions, simplifile:file_error(), binary()} |
    {cannot_start_dev_server, glisten:start_error(), integer()} |
    {cannot_start_file_watcher, gleam@otp@actor:start_error()} |
    {cannot_write_file, simplifile:file_error(), binary()} |
    {component_missing, binary()} |
    {incomplete_proxy, list(binary())} |
    {internal_error, binary()} |
    {invalid_proxy_target, binary()} |
    {main_bad_app_type,
        binary(),
        gleam@package_interface:type(),
        gleam@package_interface:type(),
        gleam@package_interface:type()} |
    {main_missing, binary()} |
    {main_takes_an_argument, binary(), gleam@package_interface:type()} |
    {module_missing, binary()} |
    {name_incorrect_type, binary(), gleam@package_interface:type()} |
    {name_missing, binary()} |
    {network_error, gleam@dynamic:dynamic_()} |
    {template_missing, binary(), simplifile:file_error()} |
    {unknown_file_error, simplifile:file_error()} |
    {unknown_platform, binary(), binary(), binary()} |
    {otp_too_old, integer()} |
    {unzip_error, gleam@dynamic:dynamic_()} |
    invalid_esbuild_binary |
    invalid_tailwind_binary.

-file("src/lustre_dev_tools/error.gleam", 79).
?DOC(false).
-spec build_error(binary()) -> binary().
build_error(Reason) ->
    Message = <<"
It looks like your project has some compilation errors that need to be addressed
before I can do anything. Here's the error message I got:

{reason}
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{reason}"/utf8>>, Reason).

-file("src/lustre_dev_tools/error.gleam", 92).
?DOC(false).
-spec bundle_error(binary()) -> binary().
bundle_error(Reason) ->
    Message = <<"
I ran into an unexpected issue while trying to bundle your project with esbuild.
Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{reason}"/utf8>>, Reason).

-file("src/lustre_dev_tools/error.gleam", 109).
?DOC(false).
-spec cannot_create_directory(simplifile:file_error(), binary()) -> binary().
cannot_create_directory(Reason, Path) ->
    Message = <<"
I ran into an error while trying to create the following directory:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{path}"/utf8>>, Path),
    gleam@string:replace(
        _pipe@1,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 130).
?DOC(false).
-spec cannot_read_file(simplifile:file_error(), binary()) -> binary().
cannot_read_file(Reason, Path) ->
    Message = <<"
I ran into an error while trying to read the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{path}"/utf8>>, Path),
    gleam@string:replace(
        _pipe@1,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 151).
?DOC(false).
-spec cannot_set_permissions(simplifile:file_error(), binary()) -> binary().
cannot_set_permissions(Reason, Path) ->
    Message = <<"
I ran into an error while trying to set the permissions on the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{path}"/utf8>>, Path),
    gleam@string:replace(
        _pipe@1,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 183).
?DOC(false).
-spec cannot_start_dev_server_default_message(glisten:start_error()) -> binary().
cannot_start_dev_server_default_message(Reason) ->
    Message = <<"
I ran into an error while trying to start the development server. Here's the
error message I got:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(
        _pipe,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 199).
?DOC(false).
-spec cannot_start_dev_server_port_in_use_message(integer()) -> binary().
cannot_start_dev_server_port_in_use_message(Port) ->
    Message = <<"
I ran into an error while trying to start the development server:
port {port} is already in use.
You can change the port to start the dev server on using the `--port` flag.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(
        _pipe,
        <<"{port}"/utf8>>,
        erlang:integer_to_binary(Port)
    ).

-file("src/lustre_dev_tools/error.gleam", 172).
?DOC(false).
-spec cannot_start_dev_server(glisten:start_error(), integer()) -> binary().
cannot_start_dev_server(Reason, Port) ->
    case Reason of
        {acceptor_failed, {abnormal, Message}} ->
            case gleam_stdlib:contains_string(Message, <<"Eaddrinuse"/utf8>>) of
                true ->
                    cannot_start_dev_server_port_in_use_message(Port);

                false ->
                    cannot_start_dev_server_default_message(Reason)
            end;

        _ ->
            cannot_start_dev_server_default_message(Reason)
    end.

-file("src/lustre_dev_tools/error.gleam", 211).
?DOC(false).
-spec cannot_start_file_watcher(gleam@otp@actor:start_error()) -> binary().
cannot_start_file_watcher(Reason) ->
    Message = <<"
I ran into an error while trying to start the file watcher used for live reloading.
Here's the error message I got:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(
        _pipe,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 227).
?DOC(false).
-spec cannot_write_file(simplifile:file_error(), binary()) -> binary().
cannot_write_file(Reason, Path) ->
    Message = <<"
I ran into an error while trying to write the following file:

    {path}

Here's the error message I got:

    {reason}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{path}"/utf8>>, Path),
    gleam@string:replace(
        _pipe@1,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 248).
?DOC(false).
-spec component_missing(binary()) -> binary().
component_missing(Module) ->
    Message = <<"
I couldn't find a valid component definition in the following module:

    {module}

To bundle a component, the module should have a public function that returns a
Lustre `App`. Try adding a function like this:

    pub const name: String = \"my-component\"

    pub fn component() -> App(Nil, Model, Msg) {
      lustre.component(init, update, view, on_attribute_change())
    }
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module).

-file("src/lustre_dev_tools/error.gleam", 269).
?DOC(false).
-spec incomplete_proxy(list(binary())) -> binary().
incomplete_proxy(Missing) ->
    Message = <<"
I'm missing some information needed to proxy requests from the development server.
The following keys are missing:

    {missing}

You can provide the missing information either as flags when starting the
development server, or by adding a `proxy` key to the `lustre-dev` section of
your `gleam.toml`.

To pass the information as flags, you should start the development server like
this:

    gleam run -m lustre/dev start --proxy-from=/api --proxy-to=http://localhost:4000/api

To add the information to your `gleam.toml`, make sure it looks something like
this:

    [lustre-dev.start]
    proxy = { from = \"/api\", to = \"http://localhost:4000/api\" }
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(
        _pipe,
        <<"{missing}"/utf8>>,
        gleam@string:join(Missing, <<", "/utf8>>)
    ).

-file("src/lustre_dev_tools/error.gleam", 297).
?DOC(false).
-spec internal_error(binary()) -> binary().
internal_error(Info) ->
    Message = <<"
Oops, it looks like I ran into an unexpected error while trying to do something.
Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
the following message:

    {info}
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{info}"/utf8>>, Info).

-file("src/lustre_dev_tools/error.gleam", 311).
?DOC(false).
-spec invalid_proxy_target(binary()) -> binary().
invalid_proxy_target(To) ->
    Message = <<"
I ran into an issue reading your proxy configuration. The URI you provided as the
target for the proxy is invalid:

    {to}

Please make sure the URI is valid and try again. If you think this is a bug,
please open an issue at https://github.com/lustre-labs/dev-tools/issues/new
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{to}"/utf8>>, To).

-file("src/lustre_dev_tools/error.gleam", 371).
?DOC(false).
-spec main_missing(binary()) -> binary().
main_missing(Module) ->
    Message = <<"
I couldn't find a `main` function in the following module:

    {module}

Is the module path correct? Your app's `main` function is the entry point we use
to build and start your app. It should look something like this:

    pub fn main() -> App(Nil, Model, Msg) {
      lustre.application(init, update, view)
    }
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module).

-file("src/lustre_dev_tools/error.gleam", 414).
?DOC(false).
-spec module_missing(binary()) -> binary().
module_missing(Module) ->
    Message = <<"
I couldn't find the following module:

    {module}

Make sure the module path is correct and also the module is not included in the
`internal_modules` list in your `gleam.toml`.

The Gleam compiler currently has a bug with it's package-interface export that
will affect lustre_dev_tools. You can find more information about that bug here:

    https://github.com/gleam-lang/gleam/issues/2898

If you know the above module exists, try running `gleam clean` and then run the
dev tools again. If you think this is a bug, please open an issue on GitHub with
some details about what you were trying to do when you ran into this issue:

    https://github.com/lustre-labs/dev-tools/issues/new
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module).

-file("src/lustre_dev_tools/error.gleam", 466).
?DOC(false).
-spec name_missing(binary()) -> binary().
name_missing(Module) ->
    Message = <<"
I couldn't find a valid component definition in the following module:

    {module}

To bundle a component, the module should have a public function that returns a
Lustre `App`. Try adding a function like this:

    pub const name: String = \"my-component\"

    pub fn component() -> App(Nil, Model, Msg) {
      lustre.component(init, update, view, on_attribute_change())
    }
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module).

-file("src/lustre_dev_tools/error.gleam", 487).
?DOC(false).
-spec network_error(gleam@dynamic:dynamic_()) -> binary().
network_error(Error) ->
    Message = <<"
I ran into an unexpected network error while trying to do something. Here's the
error message I got:

    {error}

Please check your internet connection and try again. If you think this is a bug,
please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
some details about what you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{error}"/utf8>>, gleam@string:inspect(Error)).

-file("src/lustre_dev_tools/error.gleam", 504).
?DOC(false).
-spec template_missing(binary(), simplifile:file_error()) -> binary().
template_missing(Name, Reason) ->
    Message = <<"
I ran into an unexpected error trying to read an internal template file. This
should never happen! The template file I was looking for is:

    {name}

The error message I got was:

    {reason}

Please open an issue at https://github.com/lustre-labs/dev-tools/issues/new with
the above information and some details about what you were trying to do when you
ran into this issue.
}
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{name}"/utf8>>, Name),
    gleam@string:replace(
        _pipe@1,
        <<"{reason}"/utf8>>,
        gleam@string:inspect(Reason)
    ).

-file("src/lustre_dev_tools/error.gleam", 527).
?DOC(false).
-spec unknown_file_error(simplifile:file_error()) -> binary().
unknown_file_error(Error) ->
    Message = <<"
I ran into an unexpected file system error while trying to do something. Here's
the error message I got:

    {error}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{error}"/utf8>>, gleam@string:inspect(Error)).

-file("src/lustre_dev_tools/error.gleam", 544).
?DOC(false).
-spec unknown_platform(binary(), binary(), binary()) -> binary().
unknown_platform(Binary, Os, Cpu) ->
    Path = <<"./build/.lustre/bin/"/utf8, Binary/binary>>,
    Message = <<"
I ran into a problem trying to download the {binary} binary. I couldn't find a
compatible binary for the following platform:

    OS: {os}
    CPU: {cpu}

You may be able to build the binary from source and place it at the following
path:

    {path}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{binary}"/utf8>>, Binary),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"{os}"/utf8>>, Os),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"{cpu}"/utf8>>, Cpu),
    gleam@string:replace(_pipe@3, <<"{path}"/utf8>>, Path).

-file("src/lustre_dev_tools/error.gleam", 571).
?DOC(false).
-spec otp_too_old(integer()) -> binary().
otp_too_old(Version) ->
    Message = <<"
It looks like you're running an OTP version that is not supported by the dev
tools: {version}.

You should upgrade to OTP 26 or newer to run this command:
https://gleam.run/getting-started/installing/#installing-erlang
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(
        _pipe,
        <<"{version}"/utf8>>,
        erlang:integer_to_binary(Version)
    ).

-file("src/lustre_dev_tools/error.gleam", 585).
?DOC(false).
-spec unzip_error(gleam@dynamic:dynamic_()) -> binary().
unzip_error(Error) ->
    Message = <<"
I ran into an unexpected error while trying to unzip a file. Here's the error
message I got:

    {error}

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    gleam@string:replace(_pipe, <<"{error}"/utf8>>, gleam@string:inspect(Error)).

-file("src/lustre_dev_tools/error.gleam", 602).
?DOC(false).
-spec invalid_esbuild_binary() -> binary().
invalid_esbuild_binary() ->
    <<"
It looks like the downloaded Esbuild tarball has a different hash from what I
expected.
"/utf8>>.

-file("src/lustre_dev_tools/error.gleam", 609).
?DOC(false).
-spec invalid_tailwind_binary() -> binary().
invalid_tailwind_binary() ->
    <<"
It looks like the downloaded Tailwind binary has a different hash from what I
expected.
"/utf8>>.

-file("src/lustre_dev_tools/error.gleam", 652).
?DOC(false).
-spec pretty_var(integer()) -> binary().
pretty_var(Id) ->
    case Id >= 26 of
        true ->
            <<(pretty_var((Id div 26) - 1))/binary,
                (pretty_var(Id rem 26))/binary>>;

        false ->
            Id@1 = Id + 97,
            _assert_subject = gleam@bit_array:to_string(<<Id@1/integer>>),
            {ok, Var} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                value => _assert_fail,
                                module => <<"lustre_dev_tools/error"/utf8>>,
                                function => <<"pretty_var"/utf8>>,
                                line => 658})
            end,
            Var
    end.

-file("src/lustre_dev_tools/error.gleam", 618).
?DOC(false).
-spec pretty_type(gleam@package_interface:type()) -> binary().
pretty_type(T) ->
    case T of
        {tuple, Elements} ->
            Message = <<"#({elements})"/utf8>>,
            Elements@1 = gleam@list:map(Elements, fun pretty_type/1),
            _pipe = Message,
            gleam@string:replace(
                _pipe,
                <<"{elements}"/utf8>>,
                gleam@string:join(Elements@1, <<", "/utf8>>)
            );

        {fn, Params, Return} ->
            Message@1 = <<"fn({params}) -> {return}"/utf8>>,
            Params@1 = gleam@list:map(Params, fun pretty_type/1),
            Return@1 = pretty_type(Return),
            _pipe@1 = Message@1,
            _pipe@2 = gleam@string:replace(
                _pipe@1,
                <<"{params}"/utf8>>,
                gleam@string:join(Params@1, <<", "/utf8>>)
            ),
            gleam@string:replace(_pipe@2, <<"{return}"/utf8>>, Return@1);

        {named, Name, _, _, []} ->
            Name;

        {named, Name@1, _, _, Params@2} ->
            Message@2 = <<"{name}({params})"/utf8>>,
            Params@3 = gleam@list:map(Params@2, fun pretty_type/1),
            _pipe@3 = Message@2,
            _pipe@4 = gleam@string:replace(_pipe@3, <<"{name}"/utf8>>, Name@1),
            gleam@string:replace(
                _pipe@4,
                <<"{params}"/utf8>>,
                gleam@string:join(Params@3, <<", "/utf8>>)
            );

        {variable, Id} ->
            pretty_var(Id)
    end.

-file("src/lustre_dev_tools/error.gleam", 327).
?DOC(false).
-spec main_bad_app_type(
    binary(),
    gleam@package_interface:type(),
    gleam@package_interface:type(),
    gleam@package_interface:type()
) -> binary().
main_bad_app_type(Module, Flags, Model, Msg) ->
    Message = <<"
I don't know how to serve the Lustre app returned from the `main` function in the
following module:

    {module}

I worked out your app type to be:

    App({flags}, {model}, {msg})

I need your app's flags type to either be `Nil` or a type variable like `a`. Your
`main` function should look something like this:

    pub fn main() -> App(Nil, {model}, {msg}) {
      lustre.application(init, update, view)
    }

I don't know how to produce flags of type `{flags}`! If this is intentional and
you want to provide your own flags, try modifying your `main` function to look
like this:

    pub fn main() -> Nil {
      let app = lustre.application(init, update, view)
      let flags = todo // provide your flags here
      let assert Ok() = lustre.run(app, \"#app\", flags)

      Nil
    }
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module),
    _pipe@2 = gleam@string:replace(
        _pipe@1,
        <<"{flags}"/utf8>>,
        pretty_type(Flags)
    ),
    _pipe@3 = gleam@string:replace(
        _pipe@2,
        <<"{model}"/utf8>>,
        pretty_type(Model)
    ),
    gleam@string:replace(_pipe@3, <<"{msg}"/utf8>>, pretty_type(Msg)).

-file("src/lustre_dev_tools/error.gleam", 390).
?DOC(false).
-spec main_takes_an_argument(binary(), gleam@package_interface:type()) -> binary().
main_takes_an_argument(Module, Got) ->
    Message = <<"
I ran into a problem trying to serve your Lustre app in the following module:

    {module}

I worked out the type of your `main` function to be:

    {got}

The `main` function should not take any arguments because I don't know how to
provide them! Your `main` function should look something like this:

    pub fn main() -> App(Nil, Model, Msg) {
      lustre.application(init, update, view)
    }
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module),
    gleam@string:replace(_pipe@1, <<"{got}"/utf8>>, pretty_type(Got)).

-file("src/lustre_dev_tools/error.gleam", 440).
?DOC(false).
-spec name_incorrect_type(binary(), gleam@package_interface:type()) -> binary().
name_incorrect_type(Module, Got) ->
    Message = <<"
I ran into a problem trying to bundle the component in the following module:

    {module}

The type of the `name` constant isn't what I expected. I worked out the type to
be:

    {got}

The `name` constant should be a string. Make sure it's defined like this:

    pub const name: String = \"my-component\"

If you think this is a bug, please open an issue at
https://github.com/lustre-labs/dev-tools/issues/new with some details about what
you were trying to do when you ran into this issue.
"/utf8>>,
    _pipe = Message,
    _pipe@1 = gleam@string:replace(_pipe, <<"{module}"/utf8>>, Module),
    gleam@string:replace(_pipe@1, <<"{got}"/utf8>>, pretty_type(Got)).

-file("src/lustre_dev_tools/error.gleam", 47).
?DOC(false).
-spec explain(error()) -> binary().
explain(Error) ->
    case Error of
        {build_error, Reason} ->
            build_error(Reason);

        {bundle_error, Reason@1} ->
            bundle_error(Reason@1);

        {cannot_create_directory, Reason@2, Path} ->
            cannot_create_directory(Reason@2, Path);

        {cannot_read_file, Reason@3, Path@1} ->
            cannot_read_file(Reason@3, Path@1);

        {cannot_set_permissions, Reason@4, Path@2} ->
            cannot_set_permissions(Reason@4, Path@2);

        {cannot_start_dev_server, Reason@5, Port} ->
            cannot_start_dev_server(Reason@5, Port);

        {cannot_start_file_watcher, Reason@6} ->
            cannot_start_file_watcher(Reason@6);

        {cannot_write_file, Reason@7, Path@3} ->
            cannot_write_file(Reason@7, Path@3);

        {component_missing, Module} ->
            component_missing(Module);

        {incomplete_proxy, Missing} ->
            incomplete_proxy(Missing);

        {internal_error, Message} ->
            internal_error(Message);

        {invalid_proxy_target, To} ->
            invalid_proxy_target(To);

        {main_bad_app_type, Module@1, Flags, Model, Msg} ->
            main_bad_app_type(Module@1, Flags, Model, Msg);

        {main_missing, Module@2} ->
            main_missing(Module@2);

        {main_takes_an_argument, Module@3, Got} ->
            main_takes_an_argument(Module@3, Got);

        {module_missing, Module@4} ->
            module_missing(Module@4);

        {name_incorrect_type, Module@5, Got@1} ->
            name_incorrect_type(Module@5, Got@1);

        {name_missing, Module@6} ->
            name_missing(Module@6);

        {network_error, Error@1} ->
            network_error(Error@1);

        {template_missing, Name, Reason@8} ->
            template_missing(Name, Reason@8);

        {unknown_file_error, Error@2} ->
            unknown_file_error(Error@2);

        {unknown_platform, Binary, Os, Cpu} ->
            unknown_platform(Binary, Os, Cpu);

        {otp_too_old, Version} ->
            otp_too_old(Version);

        {unzip_error, Error@3} ->
            unzip_error(Error@3);

        invalid_esbuild_binary ->
            invalid_esbuild_binary();

        invalid_tailwind_binary ->
            invalid_tailwind_binary()
    end.
