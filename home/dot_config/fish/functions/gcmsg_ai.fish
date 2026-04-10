function gcmsg_ai --description "Generate a git commit message using opencode based on staged changes"
    # Ensure we're in a git repo
    if not git rev-parse --is-inside-work-tree >/dev/null 2>&1
        echo "Not inside a git repository." >&2
        return 1
    end

    # Ensure there are staged changes
    if git diff --cached --quiet
        echo "No staged changes. Stage your changes with 'git add' first." >&2
        return 1
    end

    set diff (git diff --cached)
    set log (git log --oneline -10 2>/dev/null)
    set branch (git branch --show-current 2>/dev/null)

    set prompt "You are a git commit message generator. Based on the following git context, generate a concise and meaningful commit message following the Conventional Commits specification (e.g. feat:, fix:, chore:, refactor:, docs:, test:, style:).

Branch: $branch

Recent commits:
$log

Staged diff:
$diff

Rules:
- Output ONLY the commit message, nothing else
- First line: type(scope): short summary (max 72 chars)
- Optionally add a blank line then a short body if needed
- Use imperative mood ('add' not 'added')
- No period at the end of the subject line"

    set message (opencode run --dangerously-skip-permissions $prompt 2>/dev/null | tail -1)

    if test -z "$message"
        echo "Failed to generate commit message." >&2
        return 1
    end

    echo $message
end
