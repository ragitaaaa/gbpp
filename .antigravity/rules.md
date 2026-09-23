# Agent Execution Protocol for gbpp

## 1. Incremental Execution & Commits
- NEVER modify or refactor more than 1 logical component at a time.
- After completing a single subtask:
  1. Run the relevant test file in `tests/testthat/`.
  2. If tests pass, execute a git commit with a clear, concise message:
     `git add <specific files>`
     `git commit -m "feat/fix(scope): clear description of change"`
  3. PAUSE and ask the user for approval before moving to the next subtask.
- NEVER delete or rename files in bulk without explicit user sign-off.

## 2. Environment & Tooling
- Always use the local virtual environment Python: `.venv/Scripts/python.exe` (Windows) or `.venv/bin/python` (Unix).
- After any change to function roxygen tags or exports, execute `devtools::document()`.
- Do not introduce new external R package dependencies unless explicitly approved.