import { start } from "../lustre/lustre.mjs";

export function isIframe() {
  return window.self !== window.top;
}

export function isolatedStart(app, book) {
  while (document.body.firstChild) {
    document.body.removeChild(document.body.firstChild);
  }

  const container = document.createElement("div");
  const styles = document.createElement("style");

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

  container.setAttribute("id", "app");
  styles.textContent = css;

  // This is making use of an undocumented Lustre feature that allows us to pass
  // in any `HTMLElement` directly instead of a string CSS selector. Shh don't
  // tell anyone.
  return start(app, container, book);
}

const css = `
:host {
  /* The final boss of "please don't mess with my styles" is this list of properies
     that inherit their values by default. This is the final step in stopping any
     user styles from accidentally leaking into the fable ui. */
  border-collapse: revert;
  border-spacing: revert;
  caption-side: revert;
  color: revert;
  cursor: revert;
  direction: revert;
  empty-cells: revert;
  font-style: revert;
  font-variant: revert;
  font-weight: revert;
  font-size-adjust: revert;
  font-stretch: revert;
  font: revert;
  letter-spacing: revert;
  list-style-image: revert;
  list-style-position: revert;
  list-style-type: revert;
  list-style: revert;
  orphans: revert;
  quotes: revert;
  tab-size: revert;
  text-align: revert;
  text-align-last: revert;
  text-decoration-color: revert;
  text-indent: revert;
  text-shadow: revert;
  text-transform: revert;
  visibility: revert;
  white-space: revert;
  widows: revert;
  word-break: revert;
  word-spacing: revert;
  word-wrap: revert;

  color-scheme: light dark;

  --colour-blue-050: #f4f2fc;
  --colour-blue-100: #e8e5f9;
  --colour-blue-200: #cdc7f1;
  --colour-blue-400: #aca1e7;
  --colour-blue-600: #553fcf;
  --colour-blue-800: #3a28a1;
  --colour-blue-900: #181143;
  --colour-blue-950: #0f0b2a;

  --colour-orange-050: #fff1eb;
  --colour-orange-100: #ffe1d5;
  --colour-orange-200: #ffbda0;
  --colour-orange-400: #ff8a58;
  --colour-orange-600: #a33100;
  --colour-orange-800: #752300;
  --colour-orange-900: #310f00;
  --colour-orange-950: #1f0900;

  --colour-pink-050: #ffeffc;
  --colour-pink-100: #ffddfa;
  --colour-pink-200: #feb3f3;
  --colour-pink-400: #fe76e9;
  --colour-pink-600: #a6018c;
  --colour-pink-800: #780166;
  --colour-pink-900: #36002e;
  --colour-pink-950: #25001f;

  --colour-red-050: #fff8fa;
  --colour-red-100: #fee8ef;
  --colour-red-200: #fcbace;
  --colour-red-400: #f986a9;
  --colour-red-600: #b2093d;
  --colour-red-800: #81062c;
  --colour-red-900: #3a0314;
  --colour-red-950: #27020d;

  --colour-grey-050: #faf9f9;
  --colour-grey-100: #efedeb;
  --colour-grey-200: #d0cbc4;
  --colour-grey-400: #b2a89e;
  --colour-grey-600: #61584d;
  --colour-grey-800: #453e37;
  --colour-grey-900: #1c1916;
  --colour-grey-950: #110f0e;

  --size-gap: 0.25rem;
  --size-radius: 4px;
  --size-text: 1rem;

  color: light-dark(var(--colour-grey-900), var(--colour-grey-050));
  background-color: light-dark(var(--colour-grey-050), var(--colour-grey-900));
  margin: 0;
  font-size: var(--size-text);
  font-family: system-ui, sans-serif;
  line-height: round(var(--size-text) * 1.7, var(--size-gap));
}
  
*,
*::before,
*::after {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
}

h1,
h2,
h3,
h4,
h5,
h6 {
  font-family: var(--font-neuton);
}

button {
  background: none;
  border: none;
  font-size: inherit;
  padding: 0;
}

ul {
  list-style: none;
}

iframe {
  border: none;
}

svg.lucide {
  width: 16px;
  height: 16px;
}

/*  */

#app {
  display: grid;
  grid-template-columns: 20ch 30ch 1fr;
  min-height: 100vh;
}

.sidebar {
  border-right: 1px solid var(--colour-grey-100);
  padding: calc(var(--size-gap) * 4);
}

.story-sidebar {
  padding: calc(var(--size-gap) * 4);
  padding-block-end: 0;
}

.scene {
  padding: calc(var(--size-gap) * 4);
  padding-inline-start: 0;
  display: grid;
  grid-template-rows: auto 1fr;
  gap: calc(var(--size-gap) * 2);

  & .controls {
    display: flex;
    align-items: center;
    justify-content: end;
    gap: var(--size-gap);

    & button {
      aspect-ratio: 1 / 1;
      border-radius: var(--size-radius);
      background: var(--colour-grey-100);
      padding: 2px;
    }
  }

  & .step-count {
    font-variant-numeric: tabular-nums;
  }

  & .inner {
    background-color: white;
    border-radius: calc(var(--size-radius) * 4);
    border: 1px solid var(--colour-grey-100);
    display: flex;
    justify-content: center;
    align-items: center;
    height: 100%;
  }

  & iframe {
    width: 100%;
    height: 100%;
  }
}

.history {
  & ul {
    display: flex;
    flex-direction: column;
    gap: var(--size-gap);
  }
}

.event {
  border-radius: calc(var(--size-radius));

  &.active {
    background-color: var(--colour-blue-100);
  }

  &:hover:not(.active) {
    background-color: var(--colour-grey-100);
  }
}`;
