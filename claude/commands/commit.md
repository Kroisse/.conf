PROACTIVELY use the git-expert agent when creating git commits to ensure proper Conventional Commit formatting and commit message quality.
Check git status and recent commits to understand the current state.
If submodules exist, check their status first and commit any changes in submodules, then ask the user whether to proceed with committing the parent repository.
Analyze all staged and unstaged changes to understand what work has been done.
Draft an appropriate commit message following Conventional Commits format (type: description, with optional scope and body for complex changes).
Use "refactor" type only when code structure changes without altering functionality or behavior - avoid using it for changes that modify features or fix issues.
Stage any relevant untracked files.
Create the commit with a clear, concise message that accurately describes the changes and their purpose.
