export function applyScheme(root, scheme) {
  root.getRootNode().host.style.colorScheme = scheme;
}

export function onPrefersDark(callback) {
  const media = window.matchMedia("(prefers-color-scheme: dark)");
  const handler = () => callback(media.matches);
  media.addEventListener("change", handler);
  handler();
}

export function getItem(key) {
  return localStorage.getItem(key);
}

export function setItem(key, value) {
  localStorage.setItem(key, value);
}
