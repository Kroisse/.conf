Analyze and summarize the work done over a specified time period based on Git commits, branches, and PR discussions.

## Instructions

You should create a comprehensive work summary by:

1. **Git History Analysis**:
   - Check commits from the specified period using `git log --since="[PERIOD]"`
   - Review recent branch activity with `git branch -a --sort=-committerdate`
   - Identify key commits by the current user and team members

2. **PR and Issue Analysis**:
   - List recent PRs using `gh pr list --state all --limit 10`
   - Review active issues using `gh issue list --limit 10`
   - Check comments and discussions on relevant PRs/issues using `gh pr view` and `gh issue view`

3. **Summary Structure**:
   - **Major Work Areas**: Categorize work by feature/bugfix/infrastructure
   - **Key Achievements**: Highlight completed features and resolved issues
   - **Current Blockers**: Identify critical issues preventing progress
   - **Next Priorities**: List upcoming tasks based on discussions

4. **Output Format**:
   - Use clear headings and bullet points
   - Include relevant PR/issue numbers and links
   - Highlight critical findings with appropriate emphasis
   - Organize by priority and impact

## Usage

`/work-summary [period]`

Where `[period]` can be:
- `1 week ago` (default)
- `2 weeks ago`
- `1 month ago`
- `2024-07-01` (specific date)

## Examples

- `/work-summary` - Shows work from the last week
- `/work-summary 2 weeks ago` - Shows work from the last 2 weeks
- `/work-summary 2024-07-01` - Shows work since July 1st, 2024