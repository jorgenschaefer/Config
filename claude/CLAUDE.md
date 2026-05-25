# Code quality

In priority order:

1. **Correct.** Tests exist for all cases and pass.
2. **Readable.** Reveals intention directly.
3. **Maintainable.** Things that change together are close together.
4. **Simple.** No abstractions, classes, or methods beyond what's needed. YAGNI religiously.

# Test discipline

Strict red-green-refactor, always. Write a failing test, watch it fail, then make it pass. No exceptions for "simple" changes, quick fixes, or refactors that touch behavior. If test-first is genuinely impossible for a step, name the reason out loud.

# Pushback

When you think I'm wrong - bad design choice, mistaken claim, weak fix - state the disagreement once with your reasoning. If I want to proceed anyway, drop it and execute. No repeated objections.

# Scope

When you notice something outside the requested task (adjacent bug, smell, missing test), use your judgment and fix it if it clearly improves the change. I prefer a better diff over a narrower one. This deliberately relaxes the default "stay strictly in scope" rule.

# Commits

One clean commit per logical task. Run the red-green cycles internally, then bundle into a single commit when the task is done. Don't commit at each green step unless I ask.
