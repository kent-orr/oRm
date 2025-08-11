# AGENTS Guidelines

- Use 4 spaces for indentation; avoid tabs.
- Prefer snake_case for functions and variables; R6 class names should use PascalCase.
- Document public functions with roxygen2 comments.
- After modifying code, run tests with `R -q -e "install.packages('devtools'); devtools::test()"`, install any additional package requirements, and ensure they pass. 
- Write commit messages in the imperative mood and keep them concise.

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