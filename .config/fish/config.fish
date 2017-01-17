set fish_plugins theme git rbenv rails brew bundler gem osx pbcopy better-alias gi peco z tmux

alias emacs="env TERM=xterm-256color /usr/local/Cellar/emacs/25.1/bin/emacs -nw"
alias postgres_start="pg_ctl -l /usr/local/var/postgres/server.log start"
alias postgres_stop="pg_ctl -D /usr/local/var/postgres stop -s -m fast"
alias be="bundle exec"
alias flushdns="dscacheutil -flushcache"

set -x LESS '-g -i -M -R -S -W -z-4 -x4'
set -x PGDATA /usr/local/var/postgres
set -x PATH $HOME/.nodebrew/current/bin $HOME/.cargo/bin $PATH

function do_enter
  set -l query (commandline)

  if test -n $query
    echo
    eval $query
    commandline ''
  else
    echo
    ls
    if test (git rev-parse --is-inside-work-tree 2> /dev/null)
      echo
      echo (set_color yellow)--- git status ---(set_color normal)
      git status -sb
    end
  end
  commandline -f repaint
end

function fish_user_key_bindings
  # TODO: could not bind to enter-key because it conflicts with the original operation of enter-key.
  # bind \cm do_enter # Ctrl+m, Enter

  bind \cj do_enter # Ctrl+j
  bind \cr peco_select_history # Bind for peco history to Ctrl+r
end
