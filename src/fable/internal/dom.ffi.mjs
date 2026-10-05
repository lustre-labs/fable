export function focus(root, id) {
  root.getRootNode().getElementById(id)?.focus();
}

export function addGlobalEventListener(root, name, handler) {
  window.addEventListener(name, handler, true);

  // Keyboard events do not propagate out of an iframe.
  const listen = (iframe) => {
    iframe.contentWindow.addEventListener(name, handler, true);
  };

  root.querySelectorAll("iframe").forEach(listen);
  root.addEventListener("load", (event) => {
    if (event.target instanceof HTMLIFrameElement) listen(event.target);
  }, true);
}

export function handleEvent(event, preventDefault, stopPropagation) {
  if (preventDefault) event.preventDefault();
  if (stopPropagation) event.stopPropagation();
}
