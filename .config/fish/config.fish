status --is-interactive; and source (nodenv init -|psub)

if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
 curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

alias emacs="emacs -nw"
alias julia="/Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia"
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
alias c="curl https://corona-stats.online/Japan"

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set CHEAT_CONFIG_PATH $HOME/.config/cheat/conf.yml
set -x LESS '-g -i -M -R -S -W -z-4 -x4'
set -x PGDATA /usr/local/var/postgress
set -x PATH $HOME/nodenv/shims $HOME/.cargo/bin $HOME/.rbenv/shims (ruby -e 'print Gem.user_dir')/bin $GOPATH/bin $PATH
set -x HOMEBREW_INSTALL_CLEANUP 1

bind \cj on_enter # Ctrl+j
bind \cr peco_select_history # Bind for peco history to Ctrl+r
bind \co peco_select_ghq_repository # Ctrl + o

# The next line updates PATH for the Google Cloud SDK.
# requires installed google-cloud-sdk
# https://cloud.google.com/sdk/downloads?hl=JA#mac
if [ -f $HOME/google-cloud-sdk/path.fish.inc ]; . $HOME/google-cloud-sdk/path.fish.inc; end
