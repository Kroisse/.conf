---
description: Triage unresolved PR review comments
---

Fetch and triage unresolved review comments on the current PR.

## Steps

1. **Get PR info**: Run `gh pr view --json number,headRefName,headRepository` to identify the PR.

2. **Fetch unresolved review threads**: Use the GraphQL API to get review threads, then filter client-side (`isResolved == false`). Note: `reviewThreads` does not support server-side `filterBy`.
   ```
   gh api graphql -f query='query { repository(owner: "...", name: "...") { pullRequest(number: N) { reviewThreads(last: 50) { nodes { id isResolved comments(first: 5) { nodes { databaseId path line body author { login } } } } } } } }'
   ```
   Pipe through jq: `select(.isResolved == false)` to keep only unresolved threads.

3. **Categorize comments**: Separate bot comments (e.g. `coderabbitai[bot]`, `codecov[bot]`) from human comments. Prioritize human comments.

4. **Assess each unresolved comment**:
   - Read the relevant source code at the file and line referenced by the comment.
   - Evaluate whether the suggestion is valid, already addressed, or not applicable.
   - Classify as: **valid**, **already addressed**, **not applicable**, or **needs discussion**.

5. **Present a summary table** to the user with columns: file, comment summary, your assessment, and suggested action (reply, resolve, or leave open).

6. **Wait for user instructions** before taking any action (replying, resolving). Do not resolve or reply without explicit user approval.

## Replying and Resolving

- To reply: `gh api repos/{owner}/{repo}/pulls/{pr}/comments/{comment_id}/replies -f body='...'`
- To resolve: `gh api graphql -f query='mutation { resolveReviewThread(input: {threadId: "..."}) { thread { id isResolved } } }'`
- Write replies in concise English.
- Only resolve threads that are clearly already addressed or confirmed not applicable.
