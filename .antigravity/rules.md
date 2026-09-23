# Agent Execution Protocol for gbpp

## Standards & Guidelines
Before writing, modifying, or testing any code, read and strictly adhere to:
- [coding-standards.md](file:///c:/Projects/gbpp/.antigravity/coding-standards.md): Code style, S3 contracts, roxygen2 rules, Python safety, and numerical stability.
- [testing-guidelines.md](file:///c:/Projects/gbpp/.antigravity/testing-guidelines.md): 3-tier testing architecture, tolerance thresholds, shared fixtures, and verification sequence.

## 1. Incremental Execution & Commits
- NEVER modify or refactor more than 1 logical component at a time.
- After completing a single subtask:
  1. Run the relevant test file in `tests/testthat/`.
  2. If tests pass, execute a git commit with a clear, concise message:
     ```bash
     git add <specific files>
     git commit -m "<type>(<scope>): clear description of change"
     ```
  3. PAUSE and ask the user for approval before moving to the next subtask.
- NEVER delete or rename files in bulk without explicit user sign-off.

## 2. Environment & Tooling
- Always use the local virtual environment Python: `.venv/Scripts/python.exe` (Windows) or `.venv/bin/python` (Unix).
- After any change to function roxygen tags or exports, execute `devtools::document()`.
- Do not introduce new external R package dependencies unless explicitly approved.

## 3. Quality & Check Standards
- All changes must pass `devtools::test()` with 100% success (0 failures).
- All changes must satisfy `devtools::check(document = FALSE, error_on = 'warning')` with:
  `0 errors ✔ | 0 warnings ✔ | 0 notes ✔ | Status: OK`.