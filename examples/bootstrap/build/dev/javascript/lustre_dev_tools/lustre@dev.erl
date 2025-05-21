-module(lustre@dev).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Lustre's Dev Tools is a CLI (Command Line Interface) that provides a set of commands\n"
    " for running and building Lustre projects. If you're familiar with frontend Web\n"
    " development, you could consider the Lustre Dev Tools as something similar to\n"
    " [vite](https://vitejs.dev) but built right into the framework! If you're not\n"
    " familiar with what these tools are used for... then read on.\n"
    "\n"
    " > **Note**: currently one of lustre_dev_tools' dependencies is not compatible\n"
    " > with the most recent version of `gleam_json`, making it impossible to install.\n"
    " > To fix this, add `gleam_json = \"1.0.1\"` as a dependency in your `gleam.toml`\n"
    " > file.\n"
    "\n"
    " Lustre Dev Tools is written in Gleam and requires **Erlang** to be installed even\n"
    " if you are only building a JavaScript project. Most methods of installing Gleam\n"
    " will guide you through installing Erlang too, but make sure you have it installed\n"
    " before you try to use the Lustre CLI!\n"
    "\n"
    " Because the CLI is written in Gleam, you will run it using the `gleam` command.\n"
    " As an example, starting a development server looks like this:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev start\n"
    " ```\n"
    "\n"
    " <h2 id=\"add\" class=\"member-name\">\n"
    "   <a href=\"#add\">lustre/dev add</a>\n"
    " </h2>\n"
    "\n"
    " This command lets you install development-specific binaries and tools from outside\n"
    " the Gleam ecosystem. Lustre tries to be smart about the executables it understands:\n"
    " if you try to build a project without esbuild it will grab it, if it finds a\n"
    " tailwind.config.js it will use tailwind, and so on. All binaries are added to\n"
    " `build/.lustre/bin` in case you need to execute them manually.\n"
    "\n"
    " ### `lustre/dev add esbuild`\n"
    "\n"
    " [Esbuild](https://esbuild.github.io) is a bundler and build tool for JavaScript\n"
    " projects. This is the backbone of a lot of Lustre's build tooling and will be\n"
    " installed automatically if you use `lustre build` or `lustre dev`.\n"
    "\n"
    " Example:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev add esbuild\n"
    " ```\n"
    "\n"
    " ### `lustre add tailwind`\n"
    "\n"
    " [Tailwind CSS](https://tailwindcss.com) is a utility-first CSS framework popular\n"
    " among devs that want to quickly iterate on designs without touching CSS directly.\n"
    " This will be installed automatically if Lustre detects a `tailwind.config.js` file\n"
    " in your project.\n"
    "\n"
    " Example:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev add tailwind\n"
    " ```\n"
    "\n"
    " <h2 id=\"build\" class=\"member-name\">\n"
    "   <a href=\"#build\">lustre/dev build</a>\n"
    " </h2>\n"
    "\n"
    " Gleam projects can be compiled to JavaScript but this output is not always\n"
    " desirable for frontend projects where many individual modules can cause HTTP\n"
    " bottlenecks and slower load times. The `lustre build` command produces different\n"
    " _bundles_ that are single JavaScript files containing all the code needed for an\n"
    " application to run.\n"
    "\n"
    " If a `lustre build` subcommand is run without the necessary tooling installed,\n"
    " Lustre will attempt to install it automatically.\n"
    "\n"
    " ### `lustre/dev build app`\n"
    "\n"
    " Bundle a Gleam application into a single JavaScript file. This requires a Gleam\n"
    " module in your project with the same name as the project itself, and a public\n"
    " `main` function that will be called when the application starts.\n"
    "\n"
    " _This can be any Gleam program_, but if your `main` function returns an\n"
    " `App(Nil, model, msg)` then Lustre will automatically generate some boilerplate\n"
    " to mount the app onto an element with the id `\"app\"` and start it.\n"
    "\n"
    " In addition to bundling, Lustre's dev tools will apply the following\n"
    " transformations to the output:\n"
    "\n"
    " - FFI modules will be copied into Gleam's build directory even if they are\n"
    "   not directly under the `src/` directory. This is a temporary patch until\n"
    "   the Gleam compiler supports this itself.\n"
    "\n"
    " - FFI modules that have *relative* imports to `*.gleam` modules will have\n"
    "   their imports rewritten to point to the compiled `*.mjs` files instead.\n"
    "\n"
    " Flags:\n"
    "\n"
    " - `--minify` - Reduce the size of the output bundle by removing whitespace and\n"
    "   renaming variables. This is useful for production builds.\n"
    "\n"
    " Example:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev build app\n"
    " ```\n"
    "\n"
    " ### `lustre/dev build component`\n"
    "\n"
    " Lustre components are based on standard Web Components. This means they should\n"
    " be usable outside of Lustre and Gleam! The `lustre build component` command takes\n"
    " a module and bundles it into a single JavaScript file that can be included in\n"
    " _any_ Web app to register a new Custom Element that can be used like native HTML\n"
    " elements.\n"
    "\n"
    " For a module to be bundled as a component, it must adhere to the following rules:\n"
    "\n"
    " - There must be a `pub const name` that is a string representing the name of the\n"
    "   component to register. Remember that it must always contain a hyphen and follow\n"
    "   [these rules](https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry/define#valid_custom_element_names).\n"
    "\n"
    " - There must be a `pub fn` that has the type `fn() -> App(Nil, model, msg)`. It's\n"
    "   name is not important but in cases where multiple functions in a module fit this\n"
    "   type, the _first_ one will be used.\n"
    "\n"
    " In addition to bundling, Lustre's dev tools will apply the following\n"
    " transformations to the output:\n"
    "\n"
    " - FFI modules will be copied into Gleam's build directory even if they are\n"
    "   not directly under the `src/` directory. This is a temporary patch until\n"
    "   the Gleam compiler supports this itself.\n"
    "\n"
    " - FFI modules that have *relative* imports to `*.gleam` modules will have\n"
    "   their imports rewritten to point to the compiled `*.mjs` files instead.\n"
    "\n"
    " Arguments:\n"
    "\n"
    " - `<module_path>` - The path to the Lustre component you want to bundle. This should\n"
    "   be in the same format that you would write to import the module in a Gleam file,\n"
    "   e.g. `ui/my_componnt` and **not** `src/ui/my_component.gleam`.\n"
    "\n"
    " Flags:\n"
    "\n"
    " - `--minify` - Reduce the size of the output bundle by removing whitespace and\n"
    "   renaming variables. This is useful for production builds.\n"
    "\n"
    " Example:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev build component ui/counter\n"
    " ```\n"
    "\n"
    " <h2 id=\"start\" class=\"member-name\">\n"
    "   <a href=\"#start\">lustre/dev start</a>\n"
    " </h2>\n"
    "\n"
    " The `lustre/dev start` command starts a development server that builds and serves your\n"
    " project. This lets you focus on development without having to worry about a backend\n"
    " or additional tooling. The page will automatically reload when you make changes\n"
    " to your project.\n"
    "\n"
    " Flags:\n"
    "\n"
    " - `--port` - The port to serve the project on. Defaults to `1234`.\n"
    "\n"
    " Example:\n"
    "\n"
    " ```sh\n"
    " gleam run -m lustre/dev start --port=8080\n"
    " ```\n"
    "\n"
    " ## Getting help\n"
    "\n"
    " Lustre Dev Tools is still an experimental work in progress. If you run in to issues\n"
    " or have ideas for how it could be improved we'd love to hear from you, either by\n"
    " [opening an issue](https://github.com/lustre-labs/dev-tools/issues) or reaching out\n"
    " on the [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    "\n"
).

-file("src/lustre/dev.gleam", 193).
?DOC(
    " The `main` function is used as the entry point for Lustre's dev tools. You\n"
    " shouldn't run this function in your code, but instead use `Gleam run` to run\n"
    " this module from the command line. To see what the dev tools can do, run:\n"
    "\n"
    " ```\n"
    " gleam run -m lustre/dev -- --help\n"
    " ```\n"
).
-spec main() -> nil.
main() ->
    Args = erlang:element(4, argv:load()),
    _pipe = glint:new(),
    _pipe@1 = glint:as_module(_pipe),
    _pipe@2 = glint:with_name(_pipe@1, <<"lustre/dev"/utf8>>),
    _pipe@3 = glint:path_help(
        _pipe@2,
        [<<"add"/utf8>>],
        <<"
Commands for adding external binaries to your project. These are run and managed
by Lustre, and while not typically intended to be run manually, they can be found
inside `build/.lustre/bin`.
  "/utf8>>
    ),
    _pipe@4 = glint:add(
        _pipe@3,
        [<<"add"/utf8>>, <<"esbuild"/utf8>>],
        lustre_dev_tools@cli@add:esbuild()
    ),
    _pipe@5 = glint:add(
        _pipe@4,
        [<<"add"/utf8>>, <<"tailwind"/utf8>>],
        lustre_dev_tools@cli@add:tailwind()
    ),
    _pipe@6 = glint:add(
        _pipe@5,
        [<<"build"/utf8>>],
        lustre_dev_tools@cli@build:app()
    ),
    _pipe@7 = glint:add(
        _pipe@6,
        [<<"build"/utf8>>, <<"app"/utf8>>],
        lustre_dev_tools@cli@build:app()
    ),
    _pipe@8 = glint:add(
        _pipe@7,
        [<<"build"/utf8>>, <<"component"/utf8>>],
        lustre_dev_tools@cli@build:component()
    ),
    _pipe@9 = glint:add(
        _pipe@8,
        [<<"start"/utf8>>],
        lustre_dev_tools@cli@start:run()
    ),
    glint:run(_pipe@9, Args).
