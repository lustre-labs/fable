import { Some, None } from "../../../gleam_stdlib/gleam/option.mjs";
import { Uri } from "../../../gleam_stdlib/gleam/uri.mjs";

export const initRouter = (root, dispatch) => {
  root.addEventListener("click", (event) => {
    const a = find_anchor(event.target);

    if (!a) return;
    if (!URL.canParse(a.href)) return;

    const url = new URL(a.href);
    const uri = uri_from_url(url);
    const is_external =
      url.host !== window.location.host || a.target === "_blank";

    if (is_external) return;
    if (a.hasAttribute("download")) return;

    event.preventDefault();

    if (!is_external) {
      window.history.pushState({}, "", a.href);
      window.requestAnimationFrame(() => {
        // The browser automatically attempts to scroll to an element with a matching
        // id if a hash is present in the URL. Because we need to `preventDefault`
        // the event to prevent navigation, we also need to manually scroll to the
        // element if a
        if (url.hash) {
          document.getElementById(url.hash.slice(1))?.scrollIntoView();
        } else {
          // If no hash is present, scroll to the top of the page
          window.scrollTo(0, 0);
        }
      });
    }

    return dispatch(uri);
  });

  window.addEventListener("popstate", (e) => {
    e.preventDefault();

    const url = new URL(window.location.href);
    const uri = uri_from_url(url);

    window.requestAnimationFrame(() => {
      if (url.hash) {
        document.getElementById(url.hash.slice(1))?.scrollIntoView();
      } else {
        // If no hash is present, scroll to the top of the page
        window.scrollTo(0, 0);
      }
    });

    dispatch(uri);
  });

  window.addEventListener("modem-push", ({ detail }) => {
    dispatch(detail);
  });

  window.addEventListener("modem-replace", ({ detail }) => {
    dispatch(detail);
  });
};

const find_anchor = (el) => {
  if (!el || el.tagName === "BODY") {
    return null;
  } else if (el.tagName === "A") {
    return el;
  } else {
    return find_anchor(el.parentElement);
  }
};

const uri_from_url = (url) => {
  return new Uri(
    /* scheme   */ url.protocol
      ? new Some(url.protocol.slice(0, -1))
      : new None(),
    /* userinfo */ new None(),
    /* host     */ url.hostname ? new Some(url.hostname) : new None(),
    /* port     */ url.port ? new Some(Number(url.port)) : new None(),
    /* path     */ url.pathname,
    /* query    */ url.search ? new Some(url.search.slice(1)) : new None(),
    /* fragment */ url.hash ? new Some(url.hash.slice(1)) : new None(),
  );
};
