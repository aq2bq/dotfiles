# ==================================================
# ~/.zshrc (single-file, sectioned for future split)
# ==================================================

# --------------------------
# 00-paths
# --------------------------
# path_prepend adds to the front. List entries from low -> high priority.
typeset -U path

path_prepend() {
  [[ -d "$1" ]] && path=("$1" $path)
}

# Homebrew prefix
BREW_PREFIX="/opt/homebrew"
[[ -d "$BREW_PREFIX" ]] || BREW_PREFIX="/usr/local"

# SDK paths
export ANDROID_SDK_ROOT="$HOME/Library/Android/sdk"
export ANDROID_AVD_HOME="$HOME/.android/avd"

export XDG_CONFIG_HOME="$HOME/.config"

# Language paths
export GOPATH="${GOPATH:-$HOME/go}"
export PNPM_HOME="$HOME/Library/pnpm"
export BUN_INSTALL="$HOME/.bun"

path_prepend "$HOME/.local/bin"
path_prepend "$ANDROID_SDK_ROOT/emulator"
path_prepend "$ANDROID_SDK_ROOT/platform-tools"
path_prepend "$GOPATH/bin"
path_prepend "$HOME/.cargo/bin"
path_prepend "$BREW_PREFIX/bin"
path_prepend "$BUN_INSTALL/bin"
path_prepend "$HOME/.antigravity/antigravity/bin"
path_prepend "$PNPM_HOME/bin"
path_prepend "$BREW_PREFIX/opt/openjdk/bin"

export PATH

# --------------------------
# 10-env
# --------------------------
export CHEAT_CONFIG_PATH="$HOME/.config/cheat/conf.yml"
export LESS='-g -i -M -R -S -W -z-4 -x4'
export EDITOR=vim
export PGDATA=/usr/local/var/postgress

# --------------------------
# 20-tools init
# --------------------------
if command -v rbenv >/dev/null 2>&1; then
  eval "$(rbenv init - --no-rehash zsh)"
fi

if command -v nodenv >/dev/null 2>&1; then
  eval "$(nodenv init -)"
fi

if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then
  . "$HOME/google-cloud-sdk/path.zsh.inc"
fi

# --------------------------
# 30-functions
# --------------------------
FIGLET_FONT_DIR="$BREW_PREFIX/share/figlet/fonts"
COWSAY_COW_DIR="$BREW_PREFIX/share/cowsay/cows"

random_choice() {
  local fonts=("$FIGLET_FONT_DIR"/*.flf(N))
  (( ${#fonts[@]} )) || return 1
  local idx=$(( RANDOM % ${#fonts[@]} + 1 ))
  REPLY="${fonts[$idx]:t:r}"
  print -r -- "$REPLY"
}

random_choice_cow() {
  local cows=("$COWSAY_COW_DIR"/*.cow(N))
  (( ${#cows[@]} )) || return 1
  local idx=$(( RANDOM % ${#cows[@]} + 1 ))
  REPLY="${cows[$idx]:t:r}"
  print -r -- "$REPLY"
}

elisptest() {
  emacs -Q --batch -l "$1" -f ert-run-tests-batch-and-exit
}

# vterm_printf function for Emacs vterm
vterm_printf() {
  if [ -n "$TMUX" ] \
      && { [ "${TERM%%-*}" = "tmux" ] \
           || [ "${TERM%%-*}" = "screen" ]; }; then
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "${TERM%%-*}" = "screen" ]; then
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

# ghq + fzf
function ghq-fzf() {
  command -v ghq >/dev/null 2>&1 || return 1
  command -v fzf >/dev/null 2>&1 || return 1

  local selected
  selected=$(ghq list | fzf --query="${LBUFFER}")
  if [ -n "$selected" ]; then
    cd "$(ghq root)/$selected" || return
    zle reset-prompt
  fi
}

# on_enter function (similar to fish - Ctrl+j)
function on_enter() {
  if [ -n "$LBUFFER" ]; then
    zle accept-line
  else
    _on_enter_key=1
    zle .reset-prompt
    zle .accept-line
  fi
}

# precmd hook: run ls and git status when Ctrl+j was pressed
function on_enter_precmd() {
  if [ -n "$_on_enter_key" ]; then
    echo
    ls
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
      git status -sb
    fi
    unset _on_enter_key
  fi
}

# preexec hook: clear flag when real command is executed
function on_enter_preexec() {
  unset _on_enter_key
}


# --------------------------
# 40-aliases
# --------------------------
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs"
alias be='bundle exec'
alias diff='delta'
alias flushdns='dscacheutil -flushcache'
alias hd='hexdump -C'
alias l='less'
alias ls='lsd'
alias csv='csview'
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias gore='gore -autoimport'
alias t='open -a Typora'
alias tf='terraform'
function lgtm() {
  random_choice >/dev/null || return 1
  figlet -f "$REPLY" LGTM
}

function techie() {
  random_choice_cow >/dev/null || return 1
  cowsay -f "$REPLY" テクい！
}
alias llm='ollama run gemma3:12b'

# --------------------------
# 50-completion
# --------------------------
autoload -Uz compinit && compinit

# --------------------------
# 60-history
# --------------------------
HISTSIZE=20000
SAVEHIST=20000
HISTFILE="$HOME/.zsh_history"
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE

# --------------------------
# 70-prompt & plugins
# --------------------------
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

# zsh-autosuggestions (similar to fish autosuggestion)
if [ -f "$BREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh" ]; then
  source "$BREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
fi

# zoxide
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout default --border --cycle'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf history (custom; override fzf's default Ctrl-R widget)
# fc -rl で新しい順に取得（metafyされた履歴ファイルを直接パースしない）し、
# awkで最新の出現だけ残して重複排除。イベント番号で複数行コマンドも正しく復元。
function fzf-history-widget() {
  command -v fzf >/dev/null 2>&1 || return 1

  local selected num
  selected=$(
    fc -rl 1 |
      awk '{ cmd = $0; sub(/^[ \t]*[0-9]+\*?[ \t]+/, "", cmd); if (!seen[cmd]++) print }' |
      fzf --no-sort --exact --height 40% --layout default --border --cycle \
        --with-nth=2.. --query="$LBUFFER"
  )
  if [ -n "$selected" ]; then
    num="${selected#"${selected%%[0-9]*}"}"
    num="${num%%[^0-9]*}"
    [ -n "$num" ] && zle vi-fetch-history -n "$num"
    zle reset-prompt
  fi
}

# --------------------------
# 80-keybinds
# --------------------------
# ZLEをemacsモードに強制設定（EDITOR変数の影響を排除）
# (これをしないとtmuxの拡張キーサポートを衝突する)
bindkey -e

zle -N ghq-fzf
bindkey '^o' ghq-fzf

zle -N fzf-history-widget
bindkey '^r' fzf-history-widget

zle -N on_enter
bindkey '^j' on_enter

# --------------------------
# 90-hooks
# --------------------------
autoload -U add-zsh-hook
add-zsh-hook precmd on_enter_precmd
add-zsh-hook preexec on_enter_preexec

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"
