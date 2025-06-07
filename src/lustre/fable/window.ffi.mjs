export function match_media(query) {
  return window.matchMedia(query).matches;
}

export function watch_media(query, callback) {
  const mediaQueryList = window.matchMedia(query);
  const listener = (event) => callback(event.matches);

  mediaQueryList.addEventListener("change", listener);
}

export function add_event_listener(name, callback) {
  window.addEventListener(name, callback);
}

export function set_timeout(delay, callback) {
  return window.setTimeout(callback, delay);
}

export function cancel_timeout(id) {
  window.clearTimeout(id);
}
