function peco_src
  set -l src (ghq list --full-path | peco --query "$LBUFFER")
  if test -n $src
    cd $src
  end
end
