# AGENTS Guidelines

- Use 4 spaces for indentation; avoid tabs.
- Prefer snake_case for functions and variables; R6 class names should use PascalCase.
- Document public functions with roxygen2 comments.
- After modifying code, run tests with `R -q -e "devtools::test()"` and ensure they pass.
- Write commit messages in the imperative mood and keep them concise.
