set -x PATH /opt/homebrew/bin $HOME/.rye/shims $HOME/nodenv/shims $HOME/.cargo/bin $HOME/.rbenv/shims (ruby -e 'print Gem.user_dir')/bin $GOPATH/bin $HOME/Library/Android/sdk/platform-tools $HOME/Library/Android/sdk/emulator $HOME/.local/bin $PATH
status --is-interactive; and source (nodenv init -|psub)

# if not functions -q fisher
#   set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
#   curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
#   fish -c fisher
# end

alias be="bundle exec"
alias diff="delta"
alias flushdns="dscacheutil -flushcache"
alias hd="hexdump -C"
alias l="less"
alias ls="lsd"
alias csv="csview"
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome"
alias gore="gore -autoimport"
alias t="open -a Typora"
alias tf="terraform"
alias lgtm="figlet -f (random choice (basename -s .flf (ls /opt/homebrew/Cellar/figlet/2.2.5/share/figlet/fonts/*.flf))) LGTM"
alias techie="cowsay -f (random choice (basename -s .cow (ls /opt/homebrew/Cellar/cowsay/3.8.4/share/cowsay/cows/*.cow))) テクい！"
alias llm="ollama run gemma3:12b"

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
# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

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


# デフォルトの翻訳モデル
set -gx TRANSLATION_MODEL "hf.co/unsloth/DeepSeek-R1-0528-Qwen3-8B-GGUF:Q4_K_XL"
set -gx TRANSLATION_PROMPT "just output {LANG} translation:"

# 汎用翻訳関数
function translate
  set lang $argv[1]
  set text $argv[2..-1]

  # 言語ごとにプロンプトを動的に作成
  set prompt (string replace "{LANG}" $lang $TRANSLATION_PROMPT)

  ollama run $TRANSLATION_MODEL "$prompt $text"
end

# エイリアス（使いやすいように）
function en
  translate en $argv
end

function ja
  translate ja $argv
end
