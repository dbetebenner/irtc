# {{rPackage.name}}

> R package supporting {{user.name}}'s dissertation, "{{thesis.workingTitle}}", provisioned via [dissertation.ai](https://dissertation.ai).

This package is the **R-package half** of a two-repo dissertation environment. The NextJS app half lives at [`github.com/{{user.githubUsername}}/{{metadata.name}}`](https://github.com/{{user.githubUsername}}/{{metadata.name}}) and includes this package as a Git submodule at `packages/r-packages/{{rPackage.name}}/`.

## What's in this repo

| Path | Purpose |
|---|---|
| `R/` | Your methodological R code |
| `tests/testthat/` | Unit tests |
| `ui/www/` | The Quarto book that becomes your thesis PDF |
| `ui/www/chapters/` | Per-chapter `.qmd` sources |
| `ui/www/thesis.cls` | LaTeX thesis class (see `ui/www/THESIS-CLS-README.md` for the 3-mode strategy) |
| `ui/www/references.bib` | BibTeX bibliography |
| `.github/workflows/build-thesis.yml` | Renders thesis PDF on every push that touches `ui/www/` or `R/` |
| `.github/workflows/R-CMD-check.yml` | R package CI |
| `DESCRIPTION` | R package metadata |

## Quick start

```sh
git clone https://github.com/{{user.githubUsername}}/{{rPackage.name}}.git
cd {{rPackage.name}}

# Edit chapter content in ui/www/chapters/*.qmd
# (or open this repo in your AI-enabled editor)

# Local thesis PDF preview
cd ui/www && quarto preview

# Or push to main + check the CI-built PDF in docs/thesis.pdf
```

## Editing your dissertation

| What you want to change | Where |
|---|---|
| Thesis chapter content | `ui/www/chapters/*.qmd` |
| Bibliography | `ui/www/references.bib` |
| Thesis formatting (margins, title page, etc.) | `ui/www/thesis.cls` (see `ui/www/THESIS-CLS-README.md`) |
| Quarto book config (chapters list, PDF format, fonts) | `ui/www/_quarto.yml` |
| Methodology R code | `R/*.R` |

## The two-repo relationship

This R package + the dissertation app at `github.com/{{user.githubUsername}}/{{metadata.name}}` are designed to be edited together. Most users clone the app repo with `--recursive` to get both at once. Changes to thesis content + R code go in this repo; changes to the spec + landing page + app-level configuration go in the app repo.

## Framework links

- [dissertation.ai](https://dissertation.ai) — the Level-2 hub that provisioned this repo
- [dataimago-rpkg](https://github.com/dataimago/dataimago-rpkg) — the R generator package that this repo's structure mirrors
- [dataimago-design](https://github.com/dataimago/dataimago-design) — the design system + wiki

## License

MIT
