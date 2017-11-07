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

__prompt_command() {
    local EXIT_STATUS="$?"
    local CURRENT_DATETIME="$(date --iso-8601=seconds)"
    local GIT_REVISION="$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"

    cInvert="\[\e[7m\]"
    cReset="\[\e[0m\]"
    cBold="\[\e[1m\]"
    cUnderline="\[\e[4m\]"

    PS1="${cInvert}${EXIT_STATUS}${cReset} ${cInvert}\w${cReset} [${cBold}${GIT_REVISION}${cReset}] ${CURRENT_DATETIME}${cInvert}\n#${cReset} "
}
PROMPT_COMMAND=__prompt_command

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

which ssh-ident && alias ssh="$(which ssh-ident)"

alias ga='git add'
alias gc='git commit'
alias gca='git commit --amend'
alias gco='git checkout'
alias gd='git diff'
alias gdc='git diff --cached'
alias gl='git log'
alias gP='git push'
alias gf='git pull'
alias gs='git status'
alias tiga='tig --all'

alias d='colordiff -u'
alias dr='d -r'

PATH="$HOME/.local/bin:$HOME/.local/usr/bin:$HOME/bin${PATH:+:${PATH}}"

#PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
#PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
#PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
#PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
#PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

#export GUILE_LOAD_PATH="$HOME/src:..."

[ -f ~/.bash_private ] && source ~/.bash_private
[ -f ~/src/z/z.sh ] && source ~/src/z/z.sh
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
