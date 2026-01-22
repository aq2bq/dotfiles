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

# SDK paths
export ANDROID_SDK_ROOT="$HOME/Library/Android/sdk"
export ANDROID_AVD_HOME="$HOME/.android/avd"

# Language paths
export GOPATH="${GOPATH:-$HOME/go}"

# Optional Ruby gems path (avoid errors if Ruby is missing)
ruby_gem_bin=""
if command -v ruby >/dev/null 2>&1; then
  ruby_gem_bin="$(ruby -e 'print Gem.user_dir' 2>/dev/null)/bin"
fi

path_prepend "$HOME/.local/bin"
path_prepend "$ANDROID_SDK_ROOT/emulator"
path_prepend "$ANDROID_SDK_ROOT/platform-tools"
path_prepend "$GOPATH/bin"
path_prepend "$ruby_gem_bin"
path_prepend "$HOME/.rbenv/shims"
path_prepend "$HOME/.cargo/bin"
path_prepend "$HOME/nodenv/shims"
path_prepend /opt/homebrew/bin

# bun
export BUN_INSTALL="$HOME/.bun"
path_prepend "$BUN_INSTALL/bin"

# Antigravity
path_prepend "$HOME/.antigravity/antigravity/bin"

export PATH

# --------------------------
# 10-env
# --------------------------
export CHEAT_CONFIG_PATH="$HOME/.config/cheat/conf.yml"
export LESS='-g -i -M -R -S -W -z-4 -x4'
export EDITOR=emacsclient
export PGDATA=/usr/local/var/postgress
export HOMEBREW_INSTALL_CLEANUP=1

# --------------------------
# 20-tools init
# --------------------------
if command -v nodenv >/dev/null 2>&1; then
  eval "$(nodenv init -)"
fi

if [ -f "$HOME/google-cloud-sdk/path.zsh.inc" ]; then
  . "$HOME/google-cloud-sdk/path.zsh.inc"
fi

# --------------------------
# 30-functions
# --------------------------
BREW_PREFIX="/opt/homebrew"
[[ -d /opt/homebrew ]] || BREW_PREFIX="/usr/local"
FIGLET_FONT_DIR="$BREW_PREFIX/share/figlet/fonts"
COWSAY_COW_DIR="$BREW_PREFIX/share/cowsay/cows"

random_choice() {
  local fonts=("$FIGLET_FONT_DIR"/*.flf(N))
  (( ${#fonts[@]} )) || return 1
  local idx=$(( RANDOM % ${#fonts[@]} + 1 ))
  print -r -- "${fonts[$idx]:t:r}"
}

random_choice_cow() {
  local cows=("$COWSAY_COW_DIR"/*.cow(N))
  (( ${#cows[@]} )) || return 1
  local idx=$(( RANDOM % ${#cows[@]} + 1 ))
  print -r -- "${cows[$idx]:t:r}"
}

elisptest() {
  emacs -Q --batch -l "$1" -f ert-run-tests-batch-and-exit
}

# vterm_printf function for Emacs vterm
vterm_printf() {
  if [ -n "$TMUX" ] && [ "$TERM" = "screen" ] || [ "$TERM" = "tmux" ]; then
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
alias lgtm="figlet -f $(random_choice) LGTM"
alias techie="cowsay -f $(random_choice_cow) テクい！"
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
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY

# --------------------------
# 70-prompt & plugins
# --------------------------
if command -v starship >/dev/null 2>&1; then
  eval "$(starship init zsh)"
fi

# zsh-autosuggestions (similar to fish autosuggestion)
if [ -f /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh ]; then
  source /opt/homebrew/share/zsh-autosuggestions/zsh-autosuggestions.zsh
fi

# zoxide
if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init zsh)"
fi

# fzf
export FZF_DEFAULT_OPTS='--height 40% --layout default --border --cycle'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# fzf history (custom; override fzf's default Ctrl-R widget)
function fzf-history-widget() {
  command -v fzf >/dev/null 2>&1 || return 1

  setopt localoptions no_bang_hist

  local selected
  selected=$(
    fc -rl 1 \
      | sed 's/^[[:space:]]*[0-9]\+[[:space:]]*//' \
      | awk 'seen[$0]++ == 0' \
      | fzf --height 40% --layout default --border --cycle --query="$LBUFFER"
  )
  if [ -n "$selected" ]; then
    LBUFFER=$selected
    zle reset-prompt
  fi
}

# --------------------------
# 80-keybinds
# --------------------------
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
