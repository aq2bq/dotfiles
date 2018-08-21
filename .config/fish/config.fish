alias emacs="env TERM=xterm-256color /usr/local/Cellar/emacs/25.3/bin/emacs -nw"
alias postgres_start="pg_ctl -l /usr/local/var/postgres/server.log start"
alias postgres_stop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias be="bundle exec"
alias flushdns="dscacheutil -flushcache"
alias terminal-notifier="reattach-to-user-namespace terminal-notifier"

# https://github.com/takaaki-kasai/git-foresta
alias gf="~/bin/git-foresta | less -RSX"
alias hd="hexdump -C"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias gore="gore -autoimport"

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set -x LESS '-g -i -M -R -S -W -z-4 -x4'
set -x PGDATA /usr/local/var/postgress
set -x PATH $HOME/.nodebrew/current/bin $HOME/.cargo/bin $GOPATH/bin $PATH

function fish_user_key_bindings
  # TODO: could not bind to enter-key because it conflicts with the original operation of enter-key.
  # bind \cm do_enter # Ctrl+m, Enter
  bind \cj do_enter # Ctrl+j
  bind \cr peco_select_history # Bind for peco history to Ctrl+r
  bind \x1b peco_src # Ctrl + [
end
