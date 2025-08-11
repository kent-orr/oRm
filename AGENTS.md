# AGENTS Guidelines

- Use 4 spaces for indentation; avoid tabs.
- Prefer snake_case for functions and variables; R6 class names should use PascalCase.
- Document public functions with roxygen2 comments.
- Write commit messages in the imperative mood and keep them concise.

- If testing via testthat you will need to install on the container as well as install the packages from DESCRIPTION, RPostgre and RSQLite
.
├── AGENTS.md
├── DESCRIPTION
├── LICENSE
├── LICENSE.md
├── NAMESPACE
├── R
│   ├── Column.R
│   ├── Dialect-mysql.R
│   ├── Dialect-postgres.R
│   ├── Dialect.R
│   ├── Dialect-sqlite.R
│   ├── Engine.R
│   ├── Record.R
│   ├── Relationship.R
│   └── TableModel.R
├── README.md
├── tests
│   ├── testthat
│   └── testthat.R
└── vignettes
    ├── get_started.Rmd
    ├── using-engine.Rmd
    ├── using-records.Rmd
    ├── using-relationships.Rmd
    ├── using-tablemodels.Rmd
    ├── why_oRm.Rmd
    └── with_shiny.Rmd