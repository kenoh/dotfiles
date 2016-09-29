# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

shopt -s checkwinsize  # update rows/cols after each command
shopt -s globstar  # recursive ** expansion
shopt -s histappend  # append to histfile

export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=99999
export HISTSIZE=9999

export EDITOR="vim"

export PS1="\u@\h \e[7m\w\e[0m "

alias grep='grep --color=auto'

alias ls='ls --color --group-directories-first'
alias l='ls'
alias ll='ls -l'
alias la='ls -la'
alias lt='ll -latr'
alias cd..="cd .."

alias tmux='TERM=xterm-256color tmux'

alias e='emacsclient --no-wait'
alias et='emacsclient --tty'

alias ga='git add'
alias gc='git checkout'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log'
alias gp='git pull'
alias gs='git status'
alias tiga='tig --all'

if [ -f ~/.bash_private ]; then
	source ~/.bash_private
fi
