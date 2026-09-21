export function injectInterestingElements(shadowRoot) {
  const iframe = shadowRoot.querySelector("iframe");
  const selector = "link, script, style";

  for (const element of document.querySelectorAll(selector)) {
    if (element.tagName === "SCRIPT") {
      const script = document.createElement("script");

      for (const attribute of element.attributes) {
        script.setAttribute(attribute.name, attribute.value);
      }

      script.textContent = element.textContent;
      iframe.contentDocument.head.appendChild(script);
    } else {
      iframe.contentDocument.head.appendChild(element.cloneNode(true));
    }
  }
}
