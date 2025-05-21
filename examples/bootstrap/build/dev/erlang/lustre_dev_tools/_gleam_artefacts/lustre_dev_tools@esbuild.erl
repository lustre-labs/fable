-module(lustre_dev_tools@esbuild).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([download/2, bundle/3]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(false).

-file("src/lustre_dev_tools/esbuild.gleam", 79).
?DOC(false).
-spec check_esbuild_exists(binary()) -> boolean().
check_esbuild_exists(Path) ->
    case simplifile_erl:is_file(Path) of
        {ok, true} ->
            true;

        {ok, false} ->
            false;

        {error, _} ->
            false
    end.

-file("src/lustre_dev_tools/esbuild.gleam", 89).
?DOC(false).
-spec get_download_url_and_hash(binary(), binary()) -> {ok,
        {binary(), binary()}} |
    {error, lustre_dev_tools@error:error()}.
get_download_url_and_hash(Os, Cpu) ->
    Base = <<"https://registry.npmjs.org/@esbuild/"/utf8>>,
    case {Os, Cpu} of
        {<<"android"/utf8>>, <<"arm"/utf8>>} ->
            {ok,
                {<<Base/binary, "android-arm/-/android-arm-0.25.2.tgz"/utf8>>,
                    <<"B8DBDA0352E2EF679507DE0010C18705CBC7FCD5402ED1BD105B2707FCA4CCA1"/utf8>>}};

        {<<"android"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary,
                        "android-arm64/-/android-arm64-0.25.2.tgz"/utf8>>,
                    <<"113FD6E8D1381C1EC329E0EAB3D95361211ABAA5231C16CE2BE89B54FA9EC1F2"/utf8>>}};

        {<<"android"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "android-x64/-/android-x64-0.25.2.tgz"/utf8>>,
                    <<"0966C109B386D137C9092405B199C715C72B5CE8B9A1FA3D8C6B9D4C780FB9D8"/utf8>>}};

        {<<"darwin"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-arm64/-/darwin-arm64-0.25.2.tgz"/utf8>>,
                    <<"D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A"/utf8>>}};

        {<<"darwin"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-arm64/-/darwin-arm64-0.25.2.tgz"/utf8>>,
                    <<"D51E19BA63FE86A6FB6B70596AA0CA32362676274A50CFF6F4EF6E2DE02F4E4A"/utf8>>}};

        {<<"darwin"/utf8>>, <<"amd64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-x64/-/darwin-x64-0.25.2.tgz"/utf8>>,
                    <<"DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5"/utf8>>}};

        {<<"darwin"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "darwin-x64/-/darwin-x64-0.25.2.tgz"/utf8>>,
                    <<"DE2B564CB345FFAA6CD031A3C40920E8C3658A3321B864F53252F22D810380C5"/utf8>>}};

        {<<"freebsd"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary,
                        "freebsd-arm64/-/freebsd-arm64-0.25.2.tgz"/utf8>>,
                    <<"A8B16E6529F098CF7F8855CD2C5FBB21D740534181012AB819A4A569D9EACCDF"/utf8>>}};

        {<<"freebsd"/utf8>>, <<"amd64"/utf8>>} ->
            {ok,
                {<<Base/binary, "freebsd-x64/-/freebsd-x64-0.25.2.tgz"/utf8>>,
                    <<"B2394FBF3B85390D5D3246C50192D2B1208D83DBF96796CDC67079C66FC0AA48"/utf8>>}};

        {<<"linux"/utf8>>, <<"arm"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm/-/linux-arm-0.25.2.tgz"/utf8>>,
                    <<"9BF3844336FD30A1FCCBB6C0617572518A25E0716F98AC9E293B1D28AF97932D"/utf8>>}};

        {<<"linux"/utf8>>, <<"aarch64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64/-/linux-arm64-0.25.2.tgz"/utf8>>,
                    <<"87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282"/utf8>>}};

        {<<"linux"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-arm64/-/linux-arm64-0.25.2.tgz"/utf8>>,
                    <<"87D512B94D322CC3F008DC4EC5D9D47E82001DBEE692B825F332B157AFDF0282"/utf8>>}};

        {<<"linux"/utf8>>, <<"ia32"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-ia32/-/linux-ia32-0.25.2.tgz"/utf8>>,
                    <<"79BFB8B6B95F32C4EE8BBB21078A9E0F8DED3E148F69EAA690BCC0ABDB8D15F1"/utf8>>}};

        {<<"linux"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64/-/linux-x64-0.25.2.tgz"/utf8>>,
                    <<"52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA"/utf8>>}};

        {<<"linux"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "linux-x64/-/linux-x64-0.25.2.tgz"/utf8>>,
                    <<"52136A4B1F12F8B2567E0550B802F920593B012B22FEC64F46C99DF938466BBA"/utf8>>}};

        {<<"netbsd"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "netbsd-x64/-/netbsd-x64-0.25.2.tgz"/utf8>>,
                    <<"0F1DC50A1A688DF5D3E41EDD9DB5B425D9B23566770029ACA4FB6C79D6F1806F"/utf8>>}};

        {<<"openbsd"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary,
                        "openbsd-arm64/-/openbsd-arm64-0.25.2.tgz"/utf8>>,
                    <<"EB8B7F6BB6E56E869DA19573D5088991CA59C12870FC6180F249BA5D34163635"/utf8>>}};

        {<<"openbsd"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "openbsd-x64/-/openbsd-x64-0.25.2.tgz"/utf8>>,
                    <<"196B5E6C9E4EC7895051E49CAF59F5720B9861FE87E66776961C296AE5217A18"/utf8>>}};

        {<<"sunos"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "sunos-x64/-/sunos-x64-0.25.2.tgz"/utf8>>,
                    <<"409B456C7C341D9EF97621DA4AF84AD1CF0641B0546FE0D5D28A871C78BA2949"/utf8>>}};

        {<<"win32"/utf8>>, <<"arm64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-arm64/-/win32-arm64-0.25.2.tgz"/utf8>>,
                    <<"CAB0263FFB5CFC6B0609781EA0BF9565F0845C83802B86AB39E2107410911157"/utf8>>}};

        {<<"win32"/utf8>>, <<"ia32"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-ia32/-/win32-ia32-0.25.2.tgz"/utf8>>,
                    <<"F62A2BE084752412CD3B6603668374A844DD3F8C8C37B3EB7382CE3C0F1F1C93"/utf8>>}};

        {<<"win32"/utf8>>, <<"x64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-x64/-/win32-x64-0.25.2.tgz"/utf8>>,
                    <<"C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2"/utf8>>}};

        {<<"win32"/utf8>>, <<"x86_64"/utf8>>} ->
            {ok,
                {<<Base/binary, "win32-x64/-/win32-x64-0.25.2.tgz"/utf8>>,
                    <<"C4D5FD874A44782032FC3BF5FE77D9CCE5D8C5B1DE51D9AE8EE68B40C2FC1ED2"/utf8>>}};

        {_, _} ->
            {error, {unknown_platform, <<"esbuild"/utf8>>, Os, Cpu}}
    end.

-file("src/lustre_dev_tools/esbuild.gleam", 229).
?DOC(false).
-spec check_esbuild_integrity(bitstring(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
check_esbuild_integrity(Bin, Expected_hash) ->
    Hash = gleam@crypto:hash(sha256, Bin),
    Hash_string = gleam_stdlib:base16_encode(Hash),
    case Hash_string =:= Expected_hash of
        true ->
            {ok, nil};

        false ->
            {error, invalid_esbuild_binary}
    end.

-file("src/lustre_dev_tools/esbuild.gleam", 238).
?DOC(false).
-spec write_esbuild(bitstring(), binary(), binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
write_esbuild(Bin, Outdir, Outfile) ->
    _ = simplifile:create_directory_all(Outdir),
    _pipe = simplifile_erl:write_bits(Outfile, Bin),
    gleam@result:map_error(
        _pipe,
        fun(_capture) ->
            {cannot_write_file, _capture, filepath:join(Outdir, Outfile)}
        end
    ).

-file("src/lustre_dev_tools/esbuild.gleam", 249).
?DOC(false).
-spec set_file_permissions(binary()) -> {ok, nil} |
    {error, lustre_dev_tools@error:error()}.
set_file_permissions(File) ->
    Permissions = {file_permissions,
        gleam@set:from_list([read, write, execute]),
        gleam@set:from_list([read, execute]),
        gleam@set:from_list([read, execute])},
    _pipe = simplifile:set_permissions(File, Permissions),
    gleam@result:map_error(
        _pipe,
        fun(_capture) -> {cannot_set_permissions, _capture, File} end
    ).

-file("src/lustre_dev_tools/esbuild.gleam", 261).
?DOC(false).
-spec exec_esbuild(binary(), list(binary())) -> {ok, binary()} |
    {error, lustre_dev_tools@error:error()}.
exec_esbuild(Root, Options) ->
    _pipe = lustre_dev_tools_ffi:exec(
        <<"./build/.lustre/bin/esbuild"/utf8>>,
        Options,
        Root
    ),
    gleam@result:map_error(
        _pipe,
        fun(Pair) -> {bundle_error, erlang:element(2, Pair)} end
    ).

-file("src/lustre_dev_tools/esbuild.gleam", 219).
?DOC(false).
-spec get_esbuild(binary()) -> {ok, bitstring()} |
    {error, lustre_dev_tools@error:error()}.
get_esbuild(Url) ->
    _pipe = lustre_dev_tools_ffi:get_esbuild(Url),
    gleam@result:map_error(_pipe, fun(Field@0) -> {network_error, Field@0} end).

-file("src/lustre_dev_tools/esbuild.gleam", 224).
?DOC(false).
-spec unzip_esbuild(bitstring()) -> {ok, bitstring()} |
    {error, lustre_dev_tools@error:error()}.
unzip_esbuild(Gzip) ->
    _pipe = lustre_dev_tools_ffi:unzip_esbuild(Gzip),
    gleam@result:map_error(_pipe, fun(Field@0) -> {unzip_error, Field@0} end).

-file("src/lustre_dev_tools/esbuild.gleam", 21).
?DOC(false).
-spec download(binary(), binary()) -> lustre_dev_tools@cli:cli(nil).
download(Os, Cpu) ->
    lustre_dev_tools@cli:log(
        <<"Downloading esbuild"/utf8>>,
        fun() ->
            Outdir = filepath:join(
                lustre_dev_tools@project:root(),
                <<"build/.lustre/bin"/utf8>>
            ),
            Outfile = filepath:join(Outdir, <<"esbuild"/utf8>>),
            case check_esbuild_exists(Outfile) of
                true ->
                    lustre_dev_tools@cli:success(
                        <<"Esbuild already installed!"/utf8>>,
                        fun() -> lustre_dev_tools@cli:return(nil) end
                    );

                false ->
                    lustre_dev_tools@cli:log(
                        <<"Detecting platform"/utf8>>,
                        fun() ->
                            lustre_dev_tools@cli:'try'(
                                get_download_url_and_hash(Os, Cpu),
                                fun(_use0) ->
                                    {Url, Hash} = _use0,
                                    Max_url_size = (lustre_dev_tools@utils:term_width(
                                        
                                    )
                                    - 17)
                                    - 2,
                                    Shortened_url = lustre_dev_tools@utils:shorten_url(
                                        Url,
                                        Max_url_size
                                    ),
                                    lustre_dev_tools@cli:log(
                                        <<"Downloading from "/utf8,
                                            Shortened_url/binary>>,
                                        fun() ->
                                            lustre_dev_tools@cli:'try'(
                                                get_esbuild(Url),
                                                fun(Tarball) ->
                                                    lustre_dev_tools@cli:log(
                                                        <<"Checking the downloaded tarball"/utf8>>,
                                                        fun() ->
                                                            lustre_dev_tools@cli:'try'(
                                                                check_esbuild_integrity(
                                                                    Tarball,
                                                                    Hash
                                                                ),
                                                                fun(_) ->
                                                                    lustre_dev_tools@cli:log(
                                                                        <<"Unzipping esbuild"/utf8>>,
                                                                        fun() ->
                                                                            lustre_dev_tools@cli:'try'(
                                                                                unzip_esbuild(
                                                                                    Tarball
                                                                                ),
                                                                                fun(
                                                                                    Bin
                                                                                ) ->
                                                                                    lustre_dev_tools@cli:'try'(
                                                                                        write_esbuild(
                                                                                            Bin,
                                                                                            Outdir,
                                                                                            Outfile
                                                                                        ),
                                                                                        fun(
                                                                                            _
                                                                                        ) ->
                                                                                            lustre_dev_tools@cli:'try'(
                                                                                                set_file_permissions(
                                                                                                    Outfile
                                                                                                ),
                                                                                                fun(
                                                                                                    _
                                                                                                ) ->
                                                                                                    lustre_dev_tools@cli:success(
                                                                                                        <<"Esbuild installed!"/utf8>>,
                                                                                                        fun(
                                                                                                            
                                                                                                        ) ->
                                                                                                            lustre_dev_tools@cli:return(
                                                                                                                nil
                                                                                                            )
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            )
                                                                                        end
                                                                                    )
                                                                                end
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
            end
        end
    ).

-file("src/lustre_dev_tools/esbuild.gleam", 54).
?DOC(false).
-spec bundle(binary(), binary(), boolean()) -> lustre_dev_tools@cli:cli(nil).
bundle(Input_file, Output_file, Minify) ->
    lustre_dev_tools@cli:do(
        download(lustre_dev_tools_ffi:get_os(), lustre_dev_tools_ffi:get_cpu()),
        fun(_) ->
            lustre_dev_tools@cli:'try'(
                lustre_dev_tools@project:build(),
                fun(_) ->
                    Root = lustre_dev_tools@project:root(),
                    Flags = [<<"--bundle"/utf8>>,
                        <<"--external:node:*"/utf8>>,
                        <<"--format=esm"/utf8>>,
                        <<"--outfile="/utf8, Output_file/binary>>],
                    Options = case Minify of
                        true ->
                            [Input_file, <<"--minify"/utf8>> | Flags];

                        false ->
                            [Input_file | Flags]
                    end,
                    lustre_dev_tools@cli:log(
                        <<"Bundling with esbuild"/utf8>>,
                        fun() ->
                            lustre_dev_tools@cli:'try'(
                                exec_esbuild(Root, Options),
                                fun(_) ->
                                    lustre_dev_tools@cli:success(
                                        <<<<"Bundle produced at `"/utf8,
                                                Output_file/binary>>/binary,
                                            "`"/utf8>>,
                                        fun() ->
                                            lustre_dev_tools@cli:return(nil)
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
