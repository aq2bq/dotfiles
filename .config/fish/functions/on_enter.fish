function on_enter
    set -l query (commandline)
    if test -n $query
        echo
        eval $query
        commandline
    else
        echo
        ls
        if test (git rev-parse --is-inside-work-tree 2> /dev/null)
            git status -sb
        end
    end
    commandline -f repaint
end
