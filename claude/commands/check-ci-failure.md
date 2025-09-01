Help me investigate CI failures and identify the root cause of build errors.

## Investigation Steps

1. **Check CI status** with `gh pr view --json statusCheckRollup` to see which jobs failed
2. **Extract failed job logs** - **MUST use the git-expert agent with the Task tool** for this step:
   - Extract failed job logs using GitHub CLI commands (`gh run view`, `gh api`)
   - Use git commands to correlate failures with recent commits and changes
   - **IMPORTANT**: Always delegate GitHub CLI operations and git analysis to the git-expert agent
3. **Analyze error patterns** - Categorize failure types (compilation errors, test failures, environment issues) and provide root cause analysis with suggested fixes
