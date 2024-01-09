set -x PATH /opt/homebrew/bin $HOME/.rye/shims $HOME/nodenv/shims $HOME/.cargo/bin $HOME/.rbenv/shims (ruby -e 'print Gem.user_dir')/bin $GOPATH/bin $HOME/Library/Android/sdk/platform-tools $HOME/Library/Android/sdk/emulator $PATH
status --is-interactive; and source (nodenv init -|psub)

if not functions -q fisher
  set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
  curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
  fish -c fisher
end

# requires:
# brew tap railwaycat/emacsmacport
# brew install emacs-mac --with-modern-icon
alias emacs="/opt/homebrew/opt/emacs-mac/bin/emacs -nw"
alias julia="/Applications/Julia-1.5.app/Contents/Resources/julia/bin/julia"
alias be="bundle exec"
alias diff="delta"
alias flushdns="dscacheutil -flushcache"
alias terminal-notifier="reattach-to-user-namespace terminal-notifier"
alias gf="~/bin/git-foresta | less -RSX" # https://github.com/takaaki-kasai/git-foresta
alias hd="hexdump -C"
alias l="less"
alias ls="lsd"
alias csv="csview"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias gore="gore -autoimport"
alias t="open -a Typora"
alias re="ruby -e"
alias c="curl https://corona-stats.online/Japan"
alias tf="terraform"

set GOROOT (go env GOROOT)
set GOPATH (go env GOPATH)
set CHEAT_CONFIG_PATH $HOME/.config/cheat/conf.yml
set -x LESS '-g -i -M -R -S -W -z-4 -x4'
set -x PGDATA /usr/local/var/postgress
set -x HOMEBREW_INSTALL_CLEANUP 1
set -x ANDROID_SDK_ROOT $HOME/Library/Android/sdk
set -x ANDROID_AVD_HOME $HOME/.android/avd

set -U theme_display_date no
set -U theme_display_cmd_duration no

bind \cj on_enter # Ctrl+j
bind \cr peco_select_history # Bind for peco history to Ctrl+r
bind \co peco_select_ghq_repository # Ctrl + o

# The next line updates PATH for the Google Cloud SDK.
# requires installed google-cloud-sdk
# https://cloud.google.com/sdk/downloads?hl=JA#mac
if [ -f $HOME/google-cloud-sdk/path.fish.inc ]; . $HOME/google-cloud-sdk/path.fish.inc; end

function elisptest --description='emacs -Q --batch -l $argv -f ert-run-tests-batch-and-exit'
  emacs -Q --batch -l $argv -f ert-run-tests-batch-and-exit
end

# for Emacs: vterm
# https://github.com/akermu/emacs-libvterm#shell-side-configuration
function vterm_printf;
  if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
    # tell tmux to pass the escape sequences through
    printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
  else if string match -q -- "screen*" "$TERM"
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$argv"
  else
    printf "\e]%s\e\\" "$argv"
  end
end
