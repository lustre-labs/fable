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
-spec animate(list(lustre@vdom@vattr:attribute(WBR))) -> lustre@vdom@vnode:element(WBR).
animate(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animate"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 25).
?DOC("\n").
-spec animate_motion(list(lustre@vdom@vattr:attribute(WBV))) -> lustre@vdom@vnode:element(WBV).
animate_motion(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateMotion"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 30).
?DOC("\n").
-spec animate_transform(list(lustre@vdom@vattr:attribute(WBZ))) -> lustre@vdom@vnode:element(WBZ).
animate_transform(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"animateTransform"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 35).
?DOC("\n").
-spec mpath(list(lustre@vdom@vattr:attribute(WCD))) -> lustre@vdom@vnode:element(WCD).
mpath(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"mpath"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 40).
?DOC("\n").
-spec set(list(lustre@vdom@vattr:attribute(WCH))) -> lustre@vdom@vnode:element(WCH).
set(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"set"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 47).
?DOC("\n").
-spec circle(list(lustre@vdom@vattr:attribute(WCL))) -> lustre@vdom@vnode:element(WCL).
circle(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"circle"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 52).
?DOC("\n").
-spec ellipse(list(lustre@vdom@vattr:attribute(WCP))) -> lustre@vdom@vnode:element(WCP).
ellipse(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"ellipse"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 57).
?DOC("\n").
-spec line(list(lustre@vdom@vattr:attribute(WCT))) -> lustre@vdom@vnode:element(WCT).
line(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"line"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 62).
?DOC("\n").
-spec polygon(list(lustre@vdom@vattr:attribute(WCX))) -> lustre@vdom@vnode:element(WCX).
polygon(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polygon"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 67).
?DOC("\n").
-spec polyline(list(lustre@vdom@vattr:attribute(WDB))) -> lustre@vdom@vnode:element(WDB).
polyline(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"polyline"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 72).
?DOC("\n").
-spec rect(list(lustre@vdom@vattr:attribute(WDF))) -> lustre@vdom@vnode:element(WDF).
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
    list(lustre@vdom@vattr:attribute(WDJ)),
    list(lustre@vdom@vnode:element(WDJ))
) -> lustre@vdom@vnode:element(WDJ).
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
    list(lustre@vdom@vattr:attribute(WDP)),
    list(lustre@vdom@vnode:element(WDP))
) -> lustre@vdom@vnode:element(WDP).
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
    list(lustre@vdom@vattr:attribute(WDV)),
    list(lustre@vdom@vnode:element(WDV))
) -> lustre@vdom@vnode:element(WDV).
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
    list(lustre@vdom@vattr:attribute(WEB)),
    list(lustre@vdom@vnode:element(WEB))
) -> lustre@vdom@vnode:element(WEB).
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
    list(lustre@vdom@vattr:attribute(WEH)),
    list(lustre@vdom@vnode:element(WEH))
) -> lustre@vdom@vnode:element(WEH).
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
    list(lustre@vdom@vattr:attribute(WEN)),
    list(lustre@vdom@vnode:element(WEN))
) -> lustre@vdom@vnode:element(WEN).
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
    list(lustre@vdom@vattr:attribute(WET)),
    list(lustre@vdom@vnode:element(WET))
) -> lustre@vdom@vnode:element(WET).
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
    list(lustre@vdom@vattr:attribute(WEZ)),
    list(lustre@vdom@vnode:element(WEZ))
) -> lustre@vdom@vnode:element(WEZ).
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
    list(lustre@vdom@vattr:attribute(WFF)),
    list(lustre@vdom@vnode:element(WFF))
) -> lustre@vdom@vnode:element(WFF).
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
    list(lustre@vdom@vattr:attribute(WFL)),
    list(lustre@vdom@vnode:element(WFL))
) -> lustre@vdom@vnode:element(WFL).
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
    list(lustre@vdom@vattr:attribute(WFR)),
    list(lustre@vdom@vnode:element(WFR))
) -> lustre@vdom@vnode:element(WFR).
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
    list(lustre@vdom@vattr:attribute(WFX)),
    list(lustre@vdom@vnode:element(WFX))
) -> lustre@vdom@vnode:element(WFX).
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
    list(lustre@vdom@vattr:attribute(WGD)),
    list(lustre@vdom@vnode:element(WGD))
) -> lustre@vdom@vnode:element(WGD).
title(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"title"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 187).
?DOC("\n").
-spec fe_blend(list(lustre@vdom@vattr:attribute(WGJ))) -> lustre@vdom@vnode:element(WGJ).
fe_blend(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feBlend"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 192).
?DOC("\n").
-spec fe_color_matrix(list(lustre@vdom@vattr:attribute(WGN))) -> lustre@vdom@vnode:element(WGN).
fe_color_matrix(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feColorMatrix"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 197).
?DOC("\n").
-spec fe_component_transfer(list(lustre@vdom@vattr:attribute(WGR))) -> lustre@vdom@vnode:element(WGR).
fe_component_transfer(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComponentTransfer"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 202).
?DOC("\n").
-spec fe_composite(list(lustre@vdom@vattr:attribute(WGV))) -> lustre@vdom@vnode:element(WGV).
fe_composite(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feComposite"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 207).
?DOC("\n").
-spec fe_convolve_matrix(list(lustre@vdom@vattr:attribute(WGZ))) -> lustre@vdom@vnode:element(WGZ).
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
    list(lustre@vdom@vattr:attribute(WHD)),
    list(lustre@vdom@vnode:element(WHD))
) -> lustre@vdom@vnode:element(WHD).
fe_diffuse_lighting(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDiffuseLighting"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 220).
?DOC("\n").
-spec fe_displacement_map(list(lustre@vdom@vattr:attribute(WHJ))) -> lustre@vdom@vnode:element(WHJ).
fe_displacement_map(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDisplacementMap"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 225).
?DOC("\n").
-spec fe_drop_shadow(list(lustre@vdom@vattr:attribute(WHN))) -> lustre@vdom@vnode:element(WHN).
fe_drop_shadow(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDropShadow"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 230).
?DOC("\n").
-spec fe_flood(list(lustre@vdom@vattr:attribute(WHR))) -> lustre@vdom@vnode:element(WHR).
fe_flood(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFlood"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 235).
?DOC("\n").
-spec fe_func_a(list(lustre@vdom@vattr:attribute(WHV))) -> lustre@vdom@vnode:element(WHV).
fe_func_a(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncA"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 240).
?DOC("\n").
-spec fe_func_b(list(lustre@vdom@vattr:attribute(WHZ))) -> lustre@vdom@vnode:element(WHZ).
fe_func_b(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncB"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 245).
?DOC("\n").
-spec fe_func_g(list(lustre@vdom@vattr:attribute(WID))) -> lustre@vdom@vnode:element(WID).
fe_func_g(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncG"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 250).
?DOC("\n").
-spec fe_func_r(list(lustre@vdom@vattr:attribute(WIH))) -> lustre@vdom@vnode:element(WIH).
fe_func_r(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feFuncR"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 255).
?DOC("\n").
-spec fe_gaussian_blur(list(lustre@vdom@vattr:attribute(WIL))) -> lustre@vdom@vnode:element(WIL).
fe_gaussian_blur(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feGaussianBlur"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 260).
?DOC("\n").
-spec fe_image(list(lustre@vdom@vattr:attribute(WIP))) -> lustre@vdom@vnode:element(WIP).
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
    list(lustre@vdom@vattr:attribute(WIT)),
    list(lustre@vdom@vnode:element(WIT))
) -> lustre@vdom@vnode:element(WIT).
fe_merge(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMerge"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 273).
?DOC("\n").
-spec fe_merge_node(list(lustre@vdom@vattr:attribute(WIZ))) -> lustre@vdom@vnode:element(WIZ).
fe_merge_node(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMergeNode"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 278).
?DOC("\n").
-spec fe_morphology(list(lustre@vdom@vattr:attribute(WJD))) -> lustre@vdom@vnode:element(WJD).
fe_morphology(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feMorphology"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 283).
?DOC("\n").
-spec fe_offset(list(lustre@vdom@vattr:attribute(WJH))) -> lustre@vdom@vnode:element(WJH).
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
    list(lustre@vdom@vattr:attribute(WJL)),
    list(lustre@vdom@vnode:element(WJL))
) -> lustre@vdom@vnode:element(WJL).
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
    list(lustre@vdom@vattr:attribute(WJR)),
    list(lustre@vdom@vnode:element(WJR))
) -> lustre@vdom@vnode:element(WJR).
fe_tile(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feTile"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 304).
?DOC("\n").
-spec fe_turbulence(list(lustre@vdom@vattr:attribute(WJX))) -> lustre@vdom@vnode:element(WJX).
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
    list(lustre@vdom@vattr:attribute(WKB)),
    list(lustre@vdom@vnode:element(WKB))
) -> lustre@vdom@vnode:element(WKB).
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
    list(lustre@vdom@vattr:attribute(WKH)),
    list(lustre@vdom@vnode:element(WKH))
) -> lustre@vdom@vnode:element(WKH).
radial_gradient(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"radialGradient"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 327).
?DOC("\n").
-spec stop(list(lustre@vdom@vattr:attribute(WKN))) -> lustre@vdom@vnode:element(WKN).
stop(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"stop"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 334).
?DOC("\n").
-spec image(list(lustre@vdom@vattr:attribute(WKR))) -> lustre@vdom@vnode:element(WKR).
image(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"image"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 339).
?DOC("\n").
-spec path(list(lustre@vdom@vattr:attribute(WKV))) -> lustre@vdom@vnode:element(WKV).
path(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"path"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 344).
?DOC("\n").
-spec text(list(lustre@vdom@vattr:attribute(WKZ)), binary()) -> lustre@vdom@vnode:element(WKZ).
text(Attrs, Content) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"text"/utf8>>,
        Attrs,
        [lustre@element:text(Content)]
    ).

-file("src/lustre/element/svg.gleam", 349).
?DOC("\n").
-spec use_(list(lustre@vdom@vattr:attribute(WLD))) -> lustre@vdom@vnode:element(WLD).
use_(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"use"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 356).
?DOC("\n").
-spec fe_distant_light(list(lustre@vdom@vattr:attribute(WLH))) -> lustre@vdom@vnode:element(WLH).
fe_distant_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"feDistantLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 361).
?DOC("\n").
-spec fe_point_light(list(lustre@vdom@vattr:attribute(WLL))) -> lustre@vdom@vnode:element(WLL).
fe_point_light(Attrs) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"fePointLight"/utf8>>,
        Attrs,
        []
    ).

-file("src/lustre/element/svg.gleam", 366).
?DOC("\n").
-spec fe_spot_light(list(lustre@vdom@vattr:attribute(WLP))) -> lustre@vdom@vnode:element(WLP).
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
    list(lustre@vdom@vattr:attribute(WLT)),
    list(lustre@vdom@vnode:element(WLT))
) -> lustre@vdom@vnode:element(WLT).
clip_path(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"clipPath"/utf8>>,
        Attrs,
        Children
    ).

-file("src/lustre/element/svg.gleam", 381).
?DOC("\n").
-spec script(list(lustre@vdom@vattr:attribute(WLZ)), binary()) -> lustre@vdom@vnode:element(WLZ).
script(Attrs, Js) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"script"/utf8>>,
        Attrs,
        [lustre@element:text(Js)]
    ).

-file("src/lustre/element/svg.gleam", 386).
?DOC("\n").
-spec style(list(lustre@vdom@vattr:attribute(WMD)), binary()) -> lustre@vdom@vnode:element(WMD).
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
    list(lustre@vdom@vattr:attribute(WMH)),
    list(lustre@vdom@vnode:element(WMH))
) -> lustre@vdom@vnode:element(WMH).
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
    list(lustre@vdom@vattr:attribute(WMN)),
    list(lustre@vdom@vnode:element(WMN))
) -> lustre@vdom@vnode:element(WMN).
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
    list(lustre@vdom@vattr:attribute(WMT)),
    list(lustre@vdom@vnode:element(WMT))
) -> lustre@vdom@vnode:element(WMT).
tspan(Attrs, Children) ->
    lustre@element:namespaced(
        <<"http://www.w3.org/2000/svg"/utf8>>,
        <<"tspan"/utf8>>,
        Attrs,
        Children
    ).
