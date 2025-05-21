-module(lustre@server_component).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([element/2, script/0, route/1, method/1, include/2, subject/1, pid/1, register_subject/1, deregister_subject/1, register_callback/1, deregister_callback/1, emit/2, select/1, runtime_message_decoder/0, client_message_to_json/1]).
-export_type([transport_method/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " Server components are an advanced feature that allows you to run components\n"
    " or full Lustre applications on the server. Updates are broadcast to a small\n"
    " (10kb!) client runtime that patches the DOM and events are sent back to the\n"
    " server component in real-time.\n"
    "\n"
    " ```text\n"
    " -- SERVER -----------------------------------------------------------------\n"
    "\n"
    "                  Msg                            Element(Msg)\n"
    " +--------+        v        +----------------+        v        +------+\n"
    " |        | <-------------- |                | <-------------- |      |\n"
    " | update |                 | Lustre runtime |                 | view |\n"
    " |        | --------------> |                | --------------> |      |\n"
    " +--------+        ^        +----------------+        ^        +------+\n"
    "         #(model, Effect(msg))  |        ^          Model\n"
    "                                |        |\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    "                        +-----------------------+\n"
    "                        |                       |\n"
    "                        | Your WebSocket server |\n"
    "                        |                       |\n"
    "                        +-----------------------+\n"
    "                                |        ^\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    " -- BROWSER ----------------------------------------------------------------\n"
    "                                |        ^\n"
    "                                |        |\n"
    "                    DOM patches |        | DOM events\n"
    "                                |        |\n"
    "                                v        |\n"
    "                            +----------------+\n"
    "                            |                |\n"
    "                            | Client runtime |\n"
    "                            |                |\n"
    "                            +----------------+\n"
    " ```\n"
    "\n"
    " > **Note**: Lustre's server component runtime is separate from your application's\n"
    " > WebSocket server. You're free to bring your own stack, connect multiple\n"
    " > clients to the same Lustre instance, or keep the application alive even when\n"
    " > no clients are connected.\n"
    "\n"
    " Lustre server components run next to the rest of your backend code, your\n"
    " services, your database, etc. Real-time applications like chat services, games,\n"
    " or components that can benefit from direct access to your backend services\n"
    " like an admin dashboard or data table are excellent candidates for server\n"
    " components.\n"
    "\n"
    " ## Examples\n"
    "\n"
    " Server components are a new feature in Lustre and we're still working on the\n"
    " best ways to use them and show them off. Here are a few examples we've\n"
    " developed so far:\n"
    "\n"
    " - [Basic setup](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)\n"
    "\n"
    " - [Custom attributes and events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/02-attributes-and-events)\n"
    "\n"
    " - [Decoding DOM events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/03-event-include)\n"
    "\n"
    " - [Connecting more than one client](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/04-multiple-clients)\n"
    "\n"
    " ## Getting help\n"
    "\n"
    " If you're having trouble with Lustre or not sure what the right way to do\n"
    " something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).\n"
    " You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).\n"
    "\n"
).

-type transport_method() :: web_socket | server_sent_events | polling.

-file("src/lustre/server_component.gleam", 141).
?DOC(
    " Render the server component custom element. This element acts as the thin\n"
    " client runtime for a server component running remotely. There are a handful\n"
    " of attributes you should provide to configure the client runtime:\n"
    "\n"
    " - [`route`](#route) is the URL your server component should connect to. This\n"
    "   **must** be provided before the client runtime will do anything. The route\n"
    "   can be a relative URL, in which case it will be resolved against the current\n"
    "   page URL.\n"
    "\n"
    " - [`method`](#method) is the transport method the client runtime should use.\n"
    "   This defaults to `WebSocket` enabling duplex communication between the client\n"
    "   and server runtime. Other options include `ServerSentEvents` and `Polling`\n"
    "   which are unidirectional transports.\n"
    "\n"
    " > **Note**: the server component runtime bundle must be included and sent to\n"
    " > the client for this to work correctly. You can do this by including the\n"
    " > JavaScript bundle found in Lustre's `priv/static` directory or by inlining\n"
    " > the script source directly with the [`script`](#script) element below.\n"
).
-spec element(
    list(lustre@vdom@vattr:attribute(WZE)),
    list(lustre@vdom@vnode:element(WZE))
) -> lustre@vdom@vnode:element(WZE).
element(Attributes, Children) ->
    lustre@element:element(
        <<"lustre-server-component"/utf8>>,
        Attributes,
        Children
    ).

-file("src/lustre/server_component.gleam", 153).
?DOC(
    " Inline the server component client runtime as a `<script>` tag. Where possible\n"
    " you should prefer serving the pre-built client runtime from Lustre's `priv/static`\n"
    " directory, but this inline script can be useful for development or scenarios\n"
    " where you don't control the HTML document.\n"
).
-spec script() -> lustre@vdom@vnode:element(any()).
script() ->
    lustre@element@html:script(
        [lustre@attribute:type_(<<"module"/utf8>>)],
        <<"var Vt=5,oe=Math.pow(2,Vt),kn=oe-1,vn=oe/2,jn=oe/4;var Me=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Mn=new RegExp(`^[${Me}]*`),In=new RegExp(`[${Me}]*$`);var b=globalThis?.document,fe=\"http://www.w3.org/1999/xhtml\",pe=1,de=3,W=11,De=!!globalThis.HTMLElement?.prototype?.moveBefore;var Xe=0;var Qe=1;var Ze=2;var et=1;var tt=2;var rt=0;var nt=1;var it=2;var st=3;var ot=`\\n`,ut=\"	\";var dt=new WeakMap;async function _t(i){let e=[];for(let r of b.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of b.styleSheets)try{i.adoptedStyleSheets.push(r)}catch{try{let n=dt.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);dt.set(r,n)}i.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();i.prepend(n),t.push(n)}}return t}var ht=0;var mt=1;var xt=2;var xe=3;var $t=4;var $e=5;var ge=6;var we=7;var F=class{offset=0;#n=null;#e=()=>{};#r=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#n=e,this.#e=t,this.#r=r}mount(e){Y(this.#n,this.#p(e))}#t=[];push(e){let t=this.offset;t&&(g(e.changes,r=>{switch(r.kind){case ge:case xe:r.before=(r.before|0)+t;break;case we:case $e:r.from=(r.from|0)+t;break}}),g(e.children,r=>{r.index=(r.index|0)+t})),this.#t.push({node:this.#n,patch:e}),this.#i()}#i(){let e=this;for(;e.#t.length;){let{node:t,patch:r}=e.#t.pop();g(r.changes,n=>{switch(n.kind){case ge:e.#s(t,n.children,n.before);break;case xe:e.#o(t,n.key,n.before,n.count);break;case $t:e.#c(t,n.key,n.count);break;case we:e.#u(t,n.from,n.count);break;case $e:e.#d(t,n.from,n.count,n.with);break;case ht:e.#a(t,n.content);break;case mt:e.#f(t,n.inner_html);break;case xt:e.#m(t,n.added,n.removed);break}}),r.removed&&e.#u(t,t.childNodes.length-r.removed,r.removed),g(r.children,n=>{e.#t.push({node:R(t,n.index),patch:n})})}}#s(e,t,r){let n=bt();g(t,s=>{let c=this.#p(s);ye(e,c),Y(n,c)}),be(e,n,R(e,r))}#o(e,t,r,n){let s=yt(e,t),c=R(e,r);for(let m=0;m<n&&s!==null;++m){let p=s.nextSibling;De?e.moveBefore(s,c):be(e,s,c),s=p}}#c(e,t,r){this.#l(e,yt(e,t),r)}#u(e,t,r){this.#l(e,R(e,t),r)}#l(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,s=t[u].key;s&&e[u].keyedChildren.delete(s);for(let[c,{timeout:m}]of t[u].debouncers)clearTimeout(m);e.removeChild(t),t=n}}#d(e,t,r,n){this.#u(e,t,r);let s=this.#p(n);ye(e,s),be(e,s,R(e,t))}#a(e,t){e.data=t??\"\"}#f(e,t){e.innerHTML=t??\"\"}#m(e,t,r){g(r,n=>{let s=n.name;e[u].handlers.has(s)?(e.removeEventListener(s,kt),e[u].handlers.delete(s),e[u].throttles.has(s)&&e[u].throttles.delete(s),e[u].debouncers.has(s)&&(clearTimeout(e[u].debouncers.get(s).timeout),e[u].debouncers.delete(s))):(e.removeAttribute(s),jt[s]?.removed?.(e,s))}),g(t,n=>{this.#h(e,n)})}#p(e){switch(e.kind){case nt:{let t=gt(e);return this.#_(t,e),this.#s(t,e.children,0),t}case it:{let t=wt(e.content);return B(t,e.key),t}case rt:{let t=bt(),r=wt();return B(r,e.key),Y(t,r),g(e.children,n=>{Y(t,this.#p(n))}),t}case st:{let t=gt(e);return this.#_(t,e),this.#f(t,e.inner_html),t}}}#_(e,{attributes:t}){g(t,r=>this.#h(e,r))}#h(e,t){let r=e[u];switch(t.kind){case Xe:{let n=t.name,s=t.value??\"\";s!==e.getAttribute(n)&&e.setAttribute(n,s),jt[n]?.added?.(e,s);break}case Qe:e[t.name]=t.value;break;case Ze:{r.handlers.has(t.name)||e.addEventListener(t.name,kt,{passive:!t.prevent_default});let n=t.prevent_default,s=t.stop_propagation,c=t.immediate,m=Array.isArray(t.include)?t.include:[];if(t.limit?.kind===tt){let p=r.throttles.get(t.name)??{last:0,delay:t.limit.delay};r.throttles.set(t.name,p)}if(t.limit?.kind===et){let p=r.debouncers.get(t.name)??{timeout:null,delay:t.limit.delay};r.debouncers.set(t.name,p)}r.handlers.set(t.name,p=>{n&&p.preventDefault(),s&&p.stopPropagation();let y=p.type,$=\"\",C=p.currentTarget;for(;C!==this.#n;){let x=C[u].key,j=C.parentNode;if(x)$=`${ut}${x}${$}`;else{let Z=j.childNodes,Oe=[].indexOf.call(Z,C);j===this.#n&&(Oe-=this.offset),$=`${ot}${Oe}${$}`}C=j}$=$.slice(1);let Q=this.#r?vr(p,m):p;if(r.throttles.has(y)){let x=r.throttles.get(y),j=Date.now(),Z=x.last||0;j>Z+x.delay?(x.last=j,this.#e(Q,$,y,c)):p.preventDefault()}else if(r.debouncers.has(y)){let x=r.debouncers.get(y);clearTimeout(x.timeout),x.timeout=setTimeout(()=>{this.#e(Q,$,y,c)},x.delay)}else this.#e(Q,$,y,c)});break}}}},g=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)},Y=(i,e)=>i.appendChild(e),be=(i,e,t)=>i.insertBefore(e,t??null),gt=({key:i,tag:e,namespace:t})=>{let r=b.createElementNS(t||fe,e);return B(r,i),r},wt=i=>b.createTextNode(i??\"\"),bt=()=>b.createDocumentFragment(),R=(i,e)=>i.childNodes[e|0],u=Symbol(\"lustre\"),B=(i,e=\"\")=>{switch(i.nodeType){case pe:case W:i[u]={key:e,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case de:i[u]={key:e,debouncers:new Map};break}};var ye=(i,e)=>{if(e.nodeType===W){for(e=e.firstChild;e;e=e.nextSibling)ye(i,e);return}let t=e[u].key;t&&i[u].keyedChildren.set(t,new WeakRef(e))},yt=(i,e)=>i[u].keyedChildren.get(e).deref(),kt=i=>{let t=i.currentTarget[u].handlers.get(i.type);i.type===\"submit\"&&(i.detail??={},i.detail.formData=[...new FormData(i.target).entries()]),t(i)},vr=(i,e=[])=>{let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\"),i.type===\"submit\"&&e.push(\"detail.formData\");for(let r of e){let n=r.split(\".\");for(let s=0,c=i,m=t;s<n.length;s++){if(s===n.length-1){m[n[s]]=c[n[s]];break}m=m[n[s]]??={},c=c[n[s]]}}return t},vt=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),jr=i=>({added(e,t){e[i]=t}}),jt={checked:vt(\"checked\"),selected:vt(\"selected\"),value:jr(\"value\"),autofocus:{added(i){queueMicrotask(()=>i.focus?.())}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var Lt=0;var Ot=1;var St=2;var K=0;var Et=1;var At=2;var X=3;var ke=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#n;#e=\"ws\";#r=null;#t=null;#i=[];#s;#o=new Set;#c=new Set;#u=!1;#l=[];#d=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#u||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#t?.send({kind:K,name:r,value:n})}else t.length?this.#t?.send({kind:X,messages:t.map(([r,n])=>({kind:K,name:r,value:n}))}):this.#l.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\";for(let t of this.attributes)this.#l.push([t.name,t.value]);let e=this.getAttribute(\"route\");e&&(this.#r=new URL(e,location.href),this.#a())}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#r=new URL(r,location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#r.protocol==\"https:\"&&(this.#r.protocol=\"wss:\"),this.#r.protocol==\"http:\"&&(this.#r.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case Lt:{for(this.#n??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#n.firstChild;)this.#n.firstChild.remove();B(this.#n),this.#s=new F(this.#n,(r,n,s)=>{this.#t?.send({kind:Et,path:n,name:s,event:r})},{useServerEvents:!0}),this.#o=new Set(e.observed_attributes);let t=this.#l.filter(([r])=>this.#o.has(r));t.length&&this.#t.send({kind:X,messages:t.map(([r,n])=>({kind:K,name:r,value:n}))}),this.#l=[],this.#c=new Set(e.observed_properties);for(let r of this.#c)Object.defineProperty(this,r,{get(){return this[`_${r}`]},set(n){this[`_${r}`]=n,this.#t?.send({kind:At,name:r,value:n})}});e.will_adopt_styles&&await this.#f(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Ot:{this.#s.push(e.patch);break}case St:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#r||!this.#e)return;this.#t&&this.#t.close();let n={onConnect:()=>{this.#u=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#r,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#u=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#r,method:this.#e}})}};switch(this.#e){case\"ws\":this.#t=new ve(this.#r,n);break;case\"sse\":this.#t=new je(this.#r,n);break;case\"polling\":this.#t=new Le(this.#r,n);break}}async#f(){for(;this.#i.length;)this.#i.pop().remove(),this.#n.firstChild.remove();this.#i=await _t(this.#n),this.#s.offset=this.#i.length}},ve=class{#n;#e;#r=!1;#t=[];#i;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new WebSocket(this.#n),this.#i=t,this.#s=r,this.#o=n,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#t.length?this.#e.send(JSON.stringify({kind:X,messages:this.#t})):this.#r=!1,this.#t=[]}},this.#e.onclose=()=>{this.#o()}}send(e){if(this.#r||this.#e.readyState!==WebSocket.OPEN){this.#t.push(e);return}else this.#e.send(JSON.stringify(e)),this.#r=!0}close(){this.#e.close()}},je=class{#n;#e;#r;#t;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new EventSource(this.#n),this.#r=t,this.#t=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#t(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},Le=class{#n;#e;#r;#t;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#n=e,this.#t=t,this.#i=r,this.#s=n,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#t(),this.#r=setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#s()}#o(){return fetch(this.#n).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",ke);export{ke as ServerComponent};"/utf8>>
    ).

-file("src/lustre/server_component.gleam", 168).
?DOC(
    " The `route` attribute tells the client runtime what route it should use to\n"
    " set up the WebSocket connection to the server. Whenever this attribute is\n"
    " changed (by a clientside Lustre app, for example), the client runtime will\n"
    " destroy the current connection and set up a new one.\n"
).
-spec route(binary()) -> lustre@vdom@vattr:attribute(any()).
route(Path) ->
    lustre@attribute:attribute(<<"route"/utf8>>, Path).

-file("src/lustre/server_component.gleam", 174).
?DOC("\n").
-spec method(transport_method()) -> lustre@vdom@vattr:attribute(any()).
method(Value) ->
    lustre@attribute:attribute(<<"method"/utf8>>, case Value of
            web_socket ->
                <<"ws"/utf8>>;

            server_sent_events ->
                <<"sse"/utf8>>;

            polling ->
                <<"polling"/utf8>>
        end).

-file("src/lustre/server_component.gleam", 211).
?DOC(
    " Properties of a JavaScript event object are typically not serialisable. This\n"
    " means if we want to send them to the server we need to make a copy of any\n"
    " fields we want to decode first.\n"
    "\n"
    " This attribute tells Lustre what properties to include from an event. Properties\n"
    " can come from nested fields by using dot notation. For example, you could include\n"
    " the\n"
    " `id` of the target `element` by passing `[\"target.id\"]`.\n"
    "\n"
    " ```gleam\n"
    " import gleam/dynamic/decode\n"
    " import lustre/element.{type Element}\n"
    " import lustre/element/html\n"
    " import lustre/event\n"
    " import lustre/server_component\n"
    "\n"
    " pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {\n"
    "   let handler = fn(event) {\n"
    "     use id <- decode.at([\"target\", \"id\"], decode.string)\n"
    "     decode.success(on_click(id))\n"
    "   }\n"
    "\n"
    "   html.button(\n"
    "     [server_component.include([\"target.id\"]), event.on(\"click\", handler)],\n"
    "     [html.text(\"Click me!\")],\n"
    "   )\n"
    " }\n"
    " ```\n"
).
-spec include(lustre@vdom@vattr:attribute(WZQ), list(binary())) -> lustre@vdom@vattr:attribute(WZQ).
include(Event, Properties) ->
    case Event of
        {event, _, _, _, _, _, _, _, _, _} ->
            _record = Event,
            {event,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                Properties,
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record),
                erlang:element(10, _record)};

        _ ->
            Event
    end.

-file("src/lustre/server_component.gleam", 231).
?DOC(
    " Recover the `Subject` of the server component runtime so that it can be used\n"
    " in supervision trees or passed to other processes. If you want to hand out\n"
    " different `Subject`s to send messages to your application, take a look at the\n"
    " [`select`](#select) effect.\n"
    "\n"
    " > **Note**: this function is not available on the JavaScript target.\n"
).
-spec subject(lustre:runtime(WZU)) -> gleam@erlang@process:subject(lustre@runtime@server@runtime:message(WZU)).
subject(Runtime) ->
    gleam@function:identity(Runtime).

-file("src/lustre/server_component.gleam", 243).
?DOC(
    " Recover the `Pid` of the server component runtime so that it can be used in\n"
    " supervision trees or passed to other processes. If you want to hand out\n"
    " different `Subject`s to send messages to your application, take a look at the\n"
    " [`select`](#select) effect.\n"
    "\n"
    " > **Note**: this function is not available on the JavaScript target.\n"
).
-spec pid(lustre:runtime(any())) -> gleam@erlang@process:pid_().
pid(Runtime) ->
    _pipe = Runtime,
    _pipe@1 = subject(_pipe),
    gleam@erlang@process:subject_owner(_pipe@1).

-file("src/lustre/server_component.gleam", 258).
?DOC(
    " Register a `Subject` to receive messages and updates from Lustre's server\n"
    " component runtime. The process that owns this will be monitored and the\n"
    " subject will be gracefully removed if the process dies.\n"
    "\n"
    " > **Note**: if you are developing a server component for the JavaScript runtime,\n"
    " > you should use [`register_callback`](#register_callback) instead.\n"
).
-spec register_subject(
    gleam@erlang@process:subject(lustre@runtime@transport:client_message(XAC))
) -> lustre@runtime@server@runtime:message(XAC).
register_subject(Client) ->
    {client_registered_subject, Client}.

-file("src/lustre/server_component.gleam", 268).
?DOC(
    " Deregister a `Subject` to stop receiving messages and updates from Lustre's\n"
    " server component runtime. The subject should first have been registered with\n"
    " [`register_subject`](#register_subject) otherwise this will do nothing.\n"
).
-spec deregister_subject(
    gleam@erlang@process:subject(lustre@runtime@transport:client_message(XAG))
) -> lustre@runtime@server@runtime:message(XAG).
deregister_subject(Client) ->
    {client_deregistered_subject, Client}.

-file("src/lustre/server_component.gleam", 282).
?DOC(
    " Register a callback to be called whenever the server component runtime\n"
    " produces a message. Avoid using anonymous functions with this function, as\n"
    " they cannot later be removed using [`deregister_callback`](#deregister_callback).\n"
    "\n"
    " > **Note**: server components running on the Erlang target are **strongly**\n"
    " > encouraged to use [`register_subject`](#register_subject) instead of this\n"
    " > function.\n"
).
-spec register_callback(
    fun((lustre@runtime@transport:client_message(XAK)) -> nil)
) -> lustre@runtime@server@runtime:message(XAK).
register_callback(Callback) ->
    {client_registered_callback, Callback}.

-file("src/lustre/server_component.gleam", 296).
?DOC(
    " Deregister a callback to be called whenever the server component runtime\n"
    " produces a message. The callback to remove is determined by function equality\n"
    " and must be the same function that was passed to [`register_callback`](#register_callback).\n"
    "\n"
    " > **Note**: server components running on the Erlang target are **strongly**\n"
    " > encouraged to use [`register_subject`](#register_subject) instead of this\n"
    " > function.\n"
).
-spec deregister_callback(
    fun((lustre@runtime@transport:client_message(XAN)) -> nil)
) -> lustre@runtime@server@runtime:message(XAN).
deregister_callback(Callback) ->
    {client_deregistered_callback, Callback}.

-file("src/lustre/server_component.gleam", 312).
?DOC(
    " Instruct any connected clients to emit a DOM event with the given name and\n"
    " data. This lets your server component communicate to the frontend the same way\n"
    " any other HTML elements do: you might emit a `\"change\"` event when some part\n"
    " of the server component's state changes, for example.\n"
    "\n"
    " This is a real DOM event and any JavaScript on the page can attach an event\n"
    " listener to the server component element and listen for these events.\n"
).
-spec emit(binary(), gleam@json:json()) -> lustre@effect:effect(any()).
emit(Event, Data) ->
    lustre@effect:event(Event, Data).

-file("src/lustre/server_component.gleam", 335).
?DOC(
    " On the Erlang target, Lustre's server component runtime is an OTP\n"
    " [actor](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html) that can be\n"
    " communicated with using the standard process API and the `Subject` returned\n"
    " when starting the server component.\n"
    "\n"
    " Sometimes, you might want to hand a different `Subject` to a process to restrict\n"
    " the type of messages it can send or to distinguish messages from different\n"
    " sources from one another. The `select` effect creates a fresh `Subject` each\n"
    " time it is run. By returning a `Selector` you can teach the Lustre server\n"
    " component runtime how to listen to messages from this `Subject`.\n"
    "\n"
    " The `select` effect also gives you the dispatch function passed to `effect.from`.\n"
    " This is useful in case you want to store the provided `Subject` in your model\n"
    " for later use. For example you may subscribe to a pubsub service and later use\n"
    " that same `Subject` to unsubscribe.\n"
    "\n"
    " > **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s\n"
    " > and `Selector`s don't exist, and is the equivalent of returning `effect.none()`.\n"
).
-spec select(
    fun((fun((XAS) -> nil), gleam@erlang@process:subject(any())) -> gleam@erlang@process:selector(XAS))
) -> lustre@effect:effect(XAS).
select(Sel) ->
    lustre@effect:select(Sel).

-file("src/lustre/server_component.gleam", 348).
?DOC(
    " The server component client runtime sends JSON-encoded messages for the server\n"
    " runtime to execute. Because your own WebSocket server sits between the two\n"
    " parts of the runtime, you need to decode these actions and pass them to the\n"
    " server runtime yourself.\n"
).
-spec runtime_message_decoder() -> gleam@dynamic@decode:decoder(lustre@runtime@server@runtime:message(any())).
runtime_message_decoder() ->
    gleam@dynamic@decode:map(
        lustre@runtime@transport:server_message_decoder(),
        fun(Field@0) -> {client_dispatched_message, Field@0} end
    ).

-file("src/lustre/server_component.gleam", 364).
?DOC(
    " Encode a message you can send to the client runtime to respond to. The server\n"
    " component runtime will send messages to any registered clients to instruct\n"
    " them to update their DOM or emit events, for example.\n"
    "\n"
    " Because your WebSocket server sits between the two parts of the runtime, you\n"
    " need to encode these actions and send them to the client runtime yourself.\n"
).
-spec client_message_to_json(lustre@runtime@transport:client_message(any())) -> gleam@json:json().
client_message_to_json(Message) ->
    lustre@runtime@transport:client_message_to_json(Message).
