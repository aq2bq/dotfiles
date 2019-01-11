if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
 curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

alias emacs="env TERM=xterm-256color /usr/local/Cellar/emacs/26.1_1/bin/emacs -nw"
alias julia="/Applications/Julia-1.0.app/Contents/Resources/julia/bin/julia"
alias be="bundle exec"
alias flushdns="dscacheutil -flushcache"
alias terminal-notifier="reattach-to-user-namespace terminal-notifier"
alias gf="~/bin/git-foresta | less -RSX" # https://github.com/takaaki-kasai/git-foresta
alias hd="hexdump -C"
alias l="less"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias gore="gore -autoimport"
alias t="open -a Typora"
alias re="ruby -e"

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set -x LESS '-g -i -M -R -S -W -z-4 -x4'
set -x PGDATA /usr/local/var/postgress
set -x PATH $HOME/.nodebrew/current/bin $HOME/.cargo/bin $HOME/.rbenv/shims $GOPATH/bin $PATH

bind \cj on_enter # Ctrl+j
bind \cr peco_select_history # Bind for peco history to Ctrl+r
bind \co peco_select_ghq_repository # Ctrl + o
