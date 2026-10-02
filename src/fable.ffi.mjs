import { start } from "../lustre/lustre.mjs";

export function isIframe() {
  return window.self !== window.top;
}

export function isolatedStart(app, book) {
  while (document.body.firstChild) {
    document.body.removeChild(document.body.firstChild);
  }

  const container = element("div", { id: 'app' });
  const styles = element('link', { rel: 'stylesheet', href: '/lustre_fable/priv/fable.css' });

  // Most of our users are going to be using Lustre's dev tools. If they have any
  // static assets like stylesheets configured in their `gleam.toml` dev tools is
  // going to helpfull include all of those in the `<head>` when the user runs
  // their fable app.
  //
  // To skirt around this we're going to pull a little trick and attach a shadow
  // root directly onto the document body. This immediately isolates anything in
  // it from most of the user's styles.
  const root = document.body.attachShadow({ mode: "open" });

  // It also means we're safe to inject our own styles without any weird clashes
  // happening in the other direction.
  root.appendChild(styles);
  root.appendChild(container);

  // This is making use of an undocumented Lustre feature that allows us to pass
  // in any `HTMLElement` directly instead of a string CSS selector. Shh don't
  // tell anyone.
  return start(app, container, book);
}

function element(tagName, properties) {
  return Object.assign(document.createElement(tagName), properties)
}
