# affine-func-shiny

Shiny app to explore affine functions \(f(x) = mx + p\), with optional points
A and B on the line. Runs entirely in the browser via
[shinylive](https://posit-dev.github.io/r-shinylive/) (R compiled to
WebAssembly) — no server, no install.

**Live app:** https://goepp.github.io/affine-func-shiny/

## Structure

- `app/app.R` — the Shiny app source.
- `docs/` — static shinylive export, published via GitHub Pages. Rebuild
  after editing `app/app.R` with:

  ```r
  shinylive::export("app", "docs")
  ```
