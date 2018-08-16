# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

shopt -s checkwinsize  # update rows/cols after each command
shopt -s globstar  # recursive ** expansion
shopt -s histappend  # append to histfile
shopt -s histverify  # do not immediately execute a command from history

export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=99999
export HISTSIZE=9999

export EDITOR="vim"

cInvert="\[\e[7m\]"
cReset="\[\e[0m\]"
cBold="\[\e[1m\]"
cUnderline="\[\e[4m\]"
PS1="${cInvert}$?${cReset}\[\e]0;\w\007\] ${cInvert}\w${cReset} $(date --iso-8601=seconds)\n${cInvert}#${cReset} "

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

which ssh-ident &>/dev/null && alias ssh="$(which ssh-ident)"

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
gs() {
	git worktree list
	git status $@
}
alias tiga='tig --all'

alias d='colordiff -u'
alias dr='d -r'

alias sctl='systemctl --user'

alias openssl-cert-print-ascii='openssl x509 -text -noout -in'
PATH="$HOME/.local/bin:$HOME/.local/usr/bin:$HOME/bin${PATH:+:${PATH}}"

#PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
#PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
#PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
#PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

#export GUILE_LOAD_PATH="$HOME/src:..."

[ -f ~/.bash_private ] && source ~/.bash_private
#[ -f ~/.fzf.bash ] && source ~/.fzf.bash

