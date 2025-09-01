I need you to create a GitHub pull request by analyzing the changes in this branch.

Please PROACTIVELY use the git-expert agent to:

1. Check if a PR already exists for this branch using `gh pr view` or `gh pr list --head <branch-name>`
2. If a PR already exists, update it with `gh pr edit` instead of creating a new one
3. Analyze the differences and commit history between the current branch and origin/main
4. Use `git log origin/main..HEAD` to see all commits on this branch
5. Use `git diff origin/main...HEAD` to see the overall changes
6. Create a concise PR title that clearly describes the main purpose
7. Write a brief but comprehensive PR description that:
   - Summarizes what was changed and why
   - Highlights key improvements
   - Mentions notable technical details
   - Keeps it concise but informative
8. Use `gh pr create` to create the pull request OR `gh pr edit` to update existing PR with the generated title and description

The git-expert agent should handle all git operations and PR creation to ensure proper formatting and comprehensive analysis of the changes.
