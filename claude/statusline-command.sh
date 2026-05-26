#!/usr/bin/env bash
# Claude Code statusLine command - mirrors ~/Projects/Config/bash.sh prompt logic

input=$(cat)
cwd=$(echo "$input" | jq -r '.cwd')
model=$(echo "$input" | jq -r '.model.display_name // empty')
remaining=$(echo "$input" | jq -r '.context_window.remaining_percentage // empty')

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
    git_branch=$(git -C "$cwd" branch --no-optional-locks 2>/dev/null | sed -ne 's/^\* //p')
    if [ "$(git -C "$cwd" status --porcelain --no-optional-locks 2>/dev/null | wc -l)" -gt 0 ]; then
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

# context remaining: yellow when below 20%
if [ -n "$remaining" ]; then
    remaining_int=$(printf "%.0f" "$remaining")
    if [ "$remaining_int" -lt 20 ]; then
        add_info "$c_yellow" "ctx" "${remaining_int}%"
    else
        add_info "$c_blue"   "ctx" "${remaining_int}%"
    fi
fi

add_info "$c_blue" "model" "$model"

# --- render ---
if [ -n "$info_line" ]; then
    printf "${c_reset}[${info_line# }${c_reset}] ${c_blue}%s${c_reset}" "$cwd_info"
else
    printf "${c_blue}%s${c_reset}" "$cwd_info"
fi
