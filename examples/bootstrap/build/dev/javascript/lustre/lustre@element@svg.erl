-module(lustre@element@svg).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([animate/1, animate_motion/1, animate_transform/1, mpath/1, set/1, circle/1, ellipse/1, line/1, polygon/1, polyline/1, rect/1, a/2, defs/2, g/2, marker/2, mask/2, missing_glyph/2, pattern/2, svg/2, switch/2, symbol/2, desc/2, metadata/2, title/2, fe_blend/1, fe_color_matrix/1, fe_component_transfer/1, fe_composite/1, fe_convolve_matrix/1, fe_diffuse_lighting/2, fe_displacement_map/1, fe_drop_shadow/1, fe_flood/1, fe_func_a/1, fe_func_b/1, fe_func_g/1, fe_func_r/1, fe_gaussian_blur/1, fe_image/1, fe_merge/2, fe_merge_node/1, fe_morphology/1, fe_offset/1, fe_specular_lighting/2, fe_tile/2, fe_turbulence/1, linear_gradient/2, radial_gradient/2, stop/1, image/1, path/1, text/2, use_/1, fe_distant_light/1, fe_point_light/1, fe_spot_light/1, clip_path/2, script/2, style/2, foreign_object/2, text_path/2, tspan/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/lustre/element/svg.gleam", 20).
?DOC("\n").
-spec animate(list(lustre@vdom@vattr:attribute(UFF))) -> lustre@vdom@vnode:element(UFF).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 25).
?DOC("\n").
-spec animate_motion(list(lustre@vdom@vattr:attribute(UFJ))) -> lustre@vdom@vnode:element(UFJ).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 30).
?DOC("\n").
-spec animate_transform(list(lustre@vdom@vattr:attribute(UFN))) -> lustre@vdom@vnode:element(UFN).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 35).
?DOC("\n").
-spec mpath(list(lustre@vdom@vattr:attribute(UFR))) -> lustre@vdom@vnode:element(UFR).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 40).
?DOC("\n").
-spec set(list(lustre@vdom@vattr:attribute(UFV))) -> lustre@vdom@vnode:element(UFV).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 47).
?DOC("\n").
-spec circle(list(lustre@vdom@vattr:attribute(UFZ))) -> lustre@vdom@vnode:element(UFZ).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 52).
?DOC("\n").
-spec ellipse(list(lustre@vdom@vattr:attribute(UGD))) -> lustre@vdom@vnode:element(UGD).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 57).
?DOC("\n").
-spec line(list(lustre@vdom@vattr:attribute(UGH))) -> lustre@vdom@vnode:element(UGH).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 62).
?DOC("\n").
-spec polygon(list(lustre@vdom@vattr:attribute(UGL))) -> lustre@vdom@vnode:element(UGL).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 67).
?DOC("\n").
-spec polyline(list(lustre@vdom@vattr:attribute(UGP))) -> lustre@vdom@vnode:element(UGP).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 72).
?DOC("\n").
-spec rect(list(lustre@vdom@vattr:attribute(UGT))) -> lustre@vdom@vnode:element(UGT).
rect(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"rect"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 79).
?DOC("\n").
-spec a(
    list(lustre@vdom@vattr:attribute(UGX)),
    list(lustre@vdom@vnode:element(UGX))
) -> lustre@vdom@vnode:element(UGX).
a(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"a"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 87).
?DOC("\n").
-spec defs(
    list(lustre@vdom@vattr:attribute(UHD)),
    list(lustre@vdom@vnode:element(UHD))
) -> lustre@vdom@vnode:element(UHD).
defs(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"defs"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 95).
?DOC("\n").
-spec g(
    list(lustre@vdom@vattr:attribute(UHJ)),
    list(lustre@vdom@vnode:element(UHJ))
) -> lustre@vdom@vnode:element(UHJ).
g(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"g"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 103).
?DOC("\n").
-spec marker(
    list(lustre@vdom@vattr:attribute(UHP)),
    list(lustre@vdom@vnode:element(UHP))
) -> lustre@vdom@vnode:element(UHP).
marker(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"marker"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 111).
?DOC("\n").
-spec mask(
    list(lustre@vdom@vattr:attribute(UHV)),
    list(lustre@vdom@vnode:element(UHV))
) -> lustre@vdom@vnode:element(UHV).
mask(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mask"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 119).
?DOC("\n").
-spec missing_glyph(
    list(lustre@vdom@vattr:attribute(UIB)),
    list(lustre@vdom@vnode:element(UIB))
) -> lustre@vdom@vnode:element(UIB).
missing_glyph(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"missing-glyph"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 127).
?DOC("\n").
-spec pattern(
    list(lustre@vdom@vattr:attribute(UIH)),
    list(lustre@vdom@vnode:element(UIH))
) -> lustre@vdom@vnode:element(UIH).
pattern(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"pattern"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 135).
?DOC("\n").
-spec svg(
    list(lustre@vdom@vattr:attribute(UIN)),
    list(lustre@vdom@vnode:element(UIN))
) -> lustre@vdom@vnode:element(UIN).
svg(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"svg"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 143).
?DOC("\n").
-spec switch(
    list(lustre@vdom@vattr:attribute(UIT)),
    list(lustre@vdom@vnode:element(UIT))
) -> lustre@vdom@vnode:element(UIT).
switch(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"switch"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 151).
?DOC("\n").
-spec symbol(
    list(lustre@vdom@vattr:attribute(UIZ)),
    list(lustre@vdom@vnode:element(UIZ))
) -> lustre@vdom@vnode:element(UIZ).
symbol(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"symbol"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 161).
?DOC("\n").
-spec desc(
    list(lustre@vdom@vattr:attribute(UJF)),
    list(lustre@vdom@vnode:element(UJF))
) -> lustre@vdom@vnode:element(UJF).
desc(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"desc"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 169).
?DOC("\n").
-spec metadata(
    list(lustre@vdom@vattr:attribute(UJL)),
    list(lustre@vdom@vnode:element(UJL))
) -> lustre@vdom@vnode:element(UJL).
metadata(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"metadata"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 177).
?DOC("\n").
-spec title(
    list(lustre@vdom@vattr:attribute(UJR)),
    list(lustre@vdom@vnode:element(UJR))
) -> lustre@vdom@vnode:element(UJR).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 187).
?DOC("\n").
-spec fe_blend(list(lustre@vdom@vattr:attribute(UJX))) -> lustre@vdom@vnode:element(UJX).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 192).
?DOC("\n").
-spec fe_color_matrix(list(lustre@vdom@vattr:attribute(UKB))) -> lustre@vdom@vnode:element(UKB).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 197).
?DOC("\n").
-spec fe_component_transfer(list(lustre@vdom@vattr:attribute(UKF))) -> lustre@vdom@vnode:element(UKF).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 202).
?DOC("\n").
-spec fe_composite(list(lustre@vdom@vattr:attribute(UKJ))) -> lustre@vdom@vnode:element(UKJ).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 207).
?DOC("\n").
-spec fe_convolve_matrix(list(lustre@vdom@vattr:attribute(UKN))) -> lustre@vdom@vnode:element(UKN).
fe_convolve_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feConvolveMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 212).
?DOC("\n").
-spec fe_diffuse_lighting(
    list(lustre@vdom@vattr:attribute(UKR)),
    list(lustre@vdom@vnode:element(UKR))
) -> lustre@vdom@vnode:element(UKR).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 220).
?DOC("\n").
-spec fe_displacement_map(list(lustre@vdom@vattr:attribute(UKX))) -> lustre@vdom@vnode:element(UKX).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 225).
?DOC("\n").
-spec fe_drop_shadow(list(lustre@vdom@vattr:attribute(ULB))) -> lustre@vdom@vnode:element(ULB).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 230).
?DOC("\n").
-spec fe_flood(list(lustre@vdom@vattr:attribute(ULF))) -> lustre@vdom@vnode:element(ULF).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 235).
?DOC("\n").
-spec fe_func_a(list(lustre@vdom@vattr:attribute(ULJ))) -> lustre@vdom@vnode:element(ULJ).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 240).
?DOC("\n").
-spec fe_func_b(list(lustre@vdom@vattr:attribute(ULN))) -> lustre@vdom@vnode:element(ULN).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 245).
?DOC("\n").
-spec fe_func_g(list(lustre@vdom@vattr:attribute(ULR))) -> lustre@vdom@vnode:element(ULR).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 250).
?DOC("\n").
-spec fe_func_r(list(lustre@vdom@vattr:attribute(ULV))) -> lustre@vdom@vnode:element(ULV).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 255).
?DOC("\n").
-spec fe_gaussian_blur(list(lustre@vdom@vattr:attribute(ULZ))) -> lustre@vdom@vnode:element(ULZ).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 260).
?DOC("\n").
-spec fe_image(list(lustre@vdom@vattr:attribute(UMD))) -> lustre@vdom@vnode:element(UMD).
fe_image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feImage"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 265).
?DOC("\n").
-spec fe_merge(
    list(lustre@vdom@vattr:attribute(UMH)),
    list(lustre@vdom@vnode:element(UMH))
) -> lustre@vdom@vnode:element(UMH).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 273).
?DOC("\n").
-spec fe_merge_node(list(lustre@vdom@vattr:attribute(UMN))) -> lustre@vdom@vnode:element(UMN).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 278).
?DOC("\n").
-spec fe_morphology(list(lustre@vdom@vattr:attribute(UMR))) -> lustre@vdom@vnode:element(UMR).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 283).
?DOC("\n").
-spec fe_offset(list(lustre@vdom@vattr:attribute(UMV))) -> lustre@vdom@vnode:element(UMV).
fe_offset(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feOffset"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 288).
?DOC("\n").
-spec fe_specular_lighting(
    list(lustre@vdom@vattr:attribute(UMZ)),
    list(lustre@vdom@vnode:element(UMZ))
) -> lustre@vdom@vnode:element(UMZ).
fe_specular_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpecularLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 296).
?DOC("\n").
-spec fe_tile(
    list(lustre@vdom@vattr:attribute(UNF)),
    list(lustre@vdom@vnode:element(UNF))
) -> lustre@vdom@vnode:element(UNF).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 304).
?DOC("\n").
-spec fe_turbulence(list(lustre@vdom@vattr:attribute(UNL))) -> lustre@vdom@vnode:element(UNL).
fe_turbulence(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTurbulence"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 311).
?DOC("\n").
-spec linear_gradient(
    list(lustre@vdom@vattr:attribute(UNP)),
    list(lustre@vdom@vnode:element(UNP))
) -> lustre@vdom@vnode:element(UNP).
linear_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"linearGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 319).
?DOC("\n").
-spec radial_gradient(
    list(lustre@vdom@vattr:attribute(UNV)),
    list(lustre@vdom@vnode:element(UNV))
) -> lustre@vdom@vnode:element(UNV).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 327).
?DOC("\n").
-spec stop(list(lustre@vdom@vattr:attribute(UOB))) -> lustre@vdom@vnode:element(UOB).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 334).
?DOC("\n").
-spec image(list(lustre@vdom@vattr:attribute(UOF))) -> lustre@vdom@vnode:element(UOF).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 339).
?DOC("\n").
-spec path(list(lustre@vdom@vattr:attribute(UOJ))) -> lustre@vdom@vnode:element(UOJ).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 344).
?DOC("\n").
-spec text(list(lustre@vdom@vattr:attribute(UON)), binary()) -> lustre@vdom@vnode:element(UON).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/svg.gleam", 349).
?DOC("\n").
-spec use_(list(lustre@vdom@vattr:attribute(UOR))) -> lustre@vdom@vnode:element(UOR).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 356).
?DOC("\n").
-spec fe_distant_light(list(lustre@vdom@vattr:attribute(UOV))) -> lustre@vdom@vnode:element(UOV).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 361).
?DOC("\n").
-spec fe_point_light(list(lustre@vdom@vattr:attribute(UOZ))) -> lustre@vdom@vnode:element(UOZ).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 366).
?DOC("\n").
-spec fe_spot_light(list(lustre@vdom@vattr:attribute(UPD))) -> lustre@vdom@vnode:element(UPD).
fe_spot_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feSpotLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 373).
?DOC("\n").
-spec clip_path(
    list(lustre@vdom@vattr:attribute(UPH)),
    list(lustre@vdom@vnode:element(UPH))
) -> lustre@vdom@vnode:element(UPH).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 381).
?DOC("\n").
-spec script(list(lustre@vdom@vattr:attribute(UPN)), binary()) -> lustre@vdom@vnode:element(UPN).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("src/lustre/element/svg.gleam", 386).
?DOC("\n").
-spec style(list(lustre@vdom@vattr:attribute(UPR)), binary()) -> lustre@vdom@vnode:element(UPR).
style(Attrs, Css) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"style"/utf8>>,
        Attrs,
        [lustre@element:text(Css)]
    ).

-file("src/lustre/element/svg.gleam", 393).
?DOC("\n").
-spec foreign_object(
    list(lustre@vdom@vattr:attribute(UPV)),
    list(lustre@vdom@vnode:element(UPV))
) -> lustre@vdom@vnode:element(UPV).
foreign_object(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"foreignObject"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 401).
?DOC("\n").
-spec text_path(
    list(lustre@vdom@vattr:attribute(UQB)),
    list(lustre@vdom@vnode:element(UQB))
) -> lustre@vdom@vnode:element(UQB).
text_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"textPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 409).
?DOC("\n").
-spec tspan(
    list(lustre@vdom@vattr:attribute(UQH)),
    list(lustre@vdom@vnode:element(UQH))
) -> lustre@vdom@vnode:element(UQH).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
