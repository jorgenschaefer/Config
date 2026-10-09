#!/usr/bin/env bash
# Claude Code statusLine command - mirrors ~/Projects/Config/bash.sh prompt logic

input=$(cat)
cwd=$(echo "$input" | jq -r '.cwd')
model=$(echo "$input" | jq -r '.model.display_name // empty')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')
window_size=$(echo "$input" | jq -r '.context_window.context_window_size // empty')

# ANSI colors (same palette as bash prompt)
c_reset="\033[0m"
c_bold="\033[1m"
c_blue="\033[34m"
c_magenta="\033[35m"
c_red="\033[31m"
c_yellow="\033[33m"

# --- cwd info (shorten $HOME to ~) ---
cwd_info="${cwd/#$HOME/\~}"

# --- git info ---
git_dirty_info=""
git_clean_info=""
if git -C "$cwd" rev-parse --git-dir > /dev/null 2>&1; then
    git_branch=$(git -C "$cwd" --no-optional-locks branch 2>/dev/null | sed -ne 's/^\* //p')
    if [ "$(git -C "$cwd" --no-optional-locks status --porcelain 2>/dev/null | wc -l)" -gt 0 ]; then
        git_dirty_info="$git_branch"
    else
        git_clean_info="$git_branch"
    fi
fi

# --- assemble info segments ---
info_line=""

add_info() {
    local col="$1"
    local name="$2"
    local val="$3"
    if [ -n "$val" ]; then
        info_line="$info_line $(printf "${col}${c_bold}%s${c_reset}${col}:%s${c_reset}" "$name" "$val")"
    fi
}

add_info "$c_blue"    "git"    "$git_clean_info"
add_info "$c_magenta" "git"    "$git_dirty_info"

# context: show % used (not remaining); yellow from 150k tokens, red from 250k
# (without a window size, assume 1M tokens)
if [ -n "$remaining" ]; then
    used_int=$(awk -v r="$remaining" 'BEGIN { printf "%.0f", 100 - r }')
    used_tokens=$(awk -v r="$remaining" -v w="${window_size:-1000000}" 'BEGIN { printf "%.0f", (100 - r) * w / 100 }')
    ctx_val="${used_int}%"
    if [ "$used_tokens" -ge 250000 ]; then
        ctx_col="$c_red"
    elif [ "$used_tokens" -ge 150000 ]; then
        ctx_col="$c_yellow"
    else
        ctx_col="$c_blue"
    fi
    add_info "$ctx_col" "ctx" "$ctx_val"
fi

add_info "$c_blue" "model" "$model"

# rate limit budgets: % used, yellow when > 80%
for window in five_hour:5h seven_day:7d; do
    key="${window%%:*}"
    label="${window##*:}"
    pct=$(echo "$input" | jq -r ".rate_limits.${key}.used_percentage // empty")
    if [ -n "$pct" ]; then
        pct_int=$(awk -v p="$pct" 'BEGIN { printf "%.0f", p }')
        if [ "$pct_int" -gt 80 ]; then
            add_info "$c_yellow" "$label" "${pct_int}%"
        else
            add_info "$c_blue"   "$label" "${pct_int}%"
        fi
    fi
done

# --- render ---
if [ -n "$info_line" ]; then
    printf "%b[%b%b] %b%s%b" "$c_reset" "${info_line# }" "$c_reset" "$c_blue" "$cwd_info" "$c_reset"
else
    printf "%b%s%b" "$c_blue" "$cwd_info" "$c_reset"
fi
