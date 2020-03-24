# .bashrc
[ -f /etc/bashrc ] && . /etc/bashrc

shopt -s checkwinsize  # update rows/cols after each command
shopt -s globstar  # recursive ** expansion
shopt -s histappend  # append to histfile
shopt -s histverify  # do not immediately execute a command from history

export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=99999
export HISTSIZE=9999

export EDITOR="vim"

_c_invert="\[\e[7m\]"
_c_reset="\[\e[0m\]"
_c_bold="\[\e[1m\]"
_c_underline="\[\e[4m\]"
_exit_status="$?"
_current_datetime="$(date --iso-8601=seconds)"
[[ "$TERM" =~ ^xterm-.* ]] && _xterm_title="\[\e]0;\w\007\]"
PS1="${_c_invert}${_exit_status}${_xterm_title}${_c_reset} ${_c_invert}\w${_c_reset} ${_current_datetime}\n${_c_invert}#${_c_reset} "

alias rm='rm -i'

alias grep='grep --color=auto'

alias ls='ls --color --group-directories-first'
alias l='ls'
alias ll='ls -l'
alias la='ls -la'
alias lt='ll -latr'
alias c='pushd'
alias b='popd'
alias cd..="cd .."
alias cd...="cd ../.."
alias cd....="cd ../../.."
alias cdg='cd $(git rev-parse --show-toplevel)'
alias tree="tree -C"

alias tmux='TERM=xterm-256color tmux'

alias e='emacsclient --no-wait -e "(select-frame-set-input-focus (selected-frame))" && emacsclient --no-wait'
alias et='emacsclient --tty'

gs() { git worktree list; git status $@; }
alias ga='git add'
alias gc='git commit'
alias gca='git commit --amend'
alias gco='git checkout'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log'
alias gls='git log --graph --all --simplify-by-decoration'
alias gP='git push'
alias gf='git pull'
alias tiga='tig --all'

alias d='colordiff -u'
alias dr='d -r'

alias sctl='systemctl --user'

alias openssl-cert-print-ascii='openssl x509 -text -noout -in'

PATH="$HOME/.local/bin:$HOME/.local/usr/bin:$HOME/bin${PATH:+:${PATH}}"

[ -f ~/.bash_private ] && source ~/.bash_private
[ -f ~/src/z/z.sh ] && source ~/src/z/z.sh
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
