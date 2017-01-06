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
  bind \cj do_enter
end