# Thesis class file (`thesis.cls`)

Your dissertation's `thesis.cls` controls how the thesis PDF is formatted — margins, title page, signature page, citation style, front-matter ordering.

## Which mode are you in?

Check your `dataimago-spec.yaml`'s `vertical.dissertation.thesis.classFile.type`:

- **`shipped`** — the framework's default (this file as-shipped). Works for many institutions; no action needed.
- **`vendored`** — your institution provides an official `.cls`. Replace this `thesis.cls` with the institution-provided file. Commit + push; the next `build-thesis.yml` run will use it.
- **`generated`** — your institution provides written formatting guidelines (committed to `<repo-root>/context/thesis-formatting/`). AI assistance in this repo will help iterate on `thesis.cls` to match those requirements. See "Iterating on a generated thesis.cls" below.

## Iterating on a generated `thesis.cls` (generated mode)

Your formatting guidelines are at `context/thesis-formatting/<filename>`. Recommended workflow:

1. **Open this repo in an AI-enabled editor** (Claude Code, Cursor, etc.).
2. **Ask the AI:** *"Read `context/thesis-formatting/<filename>` and update `ui/www/thesis.cls` to match the institutional requirements."*
3. **Push your changes** — `build-thesis.yml` will rebuild the PDF.
4. **Open the rebuilt PDF** at `docs/thesis.pdf`, compare to the institutional sample (if any), and iterate.

Most institutional formatting differs from the framework default in:

- **Margins** (typical: 1.5in left for binding, 1in elsewhere)
- **Title page layout** (text positioning, capitalization conventions, institutional logo placement)
- **Signature page** (committee names + degree program text + signature line spacing)
- **Chapter heading style + spacing** (some institutions require specific point sizes / fonts for chapter titles)
- **Specific front-matter ordering** (Dedication → Acknowledgments → Abstract → ToC is common; some institutions require Abstract before everything)
- **Citation style** (APA / Chicago / institution-specific)
- **Specific font** (some institutions require Times New Roman; the framework default uses Noto Sans via `fontspec`)

## Switching modes

You can switch modes anytime by editing `dataimago-spec.yaml`'s `vertical.dissertation.thesis.classFile.type` and pushing.

## What's in the framework default `thesis.cls`

The file ships with sane defaults derived from the `book` class:

- 1in margins (typical for non-binding submission)
- One-and-a-half line spacing
- Noto Sans body font (via `fontspec`; XeLaTeX-only)
- `\thesistitle{...}{...}{...}{...}{...}` — title / author / institution / degree program / year
- `\signaturepage{...}` — wraps the committee approval block
- `\sigline{...}` — single signature line helper
- `\begin{abstract}...\end{abstract}` — committee-formatted abstract environment

If your institution requires something different, the most common adjustments are 1-2 line changes (margins, font, spacing) which AI assistance can make easily.

## Adding to `dataimago.sty`

The companion `dataimago.sty` file holds LaTeX-level typography hooks. Most users won't need to edit it; institution-specific overrides are best made in `thesis.cls`.
