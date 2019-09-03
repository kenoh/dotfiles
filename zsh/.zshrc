# -*- mode: sh; -*-
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=99999
SAVEHIST=99999
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
autoload -U zargs
compinit
# End of lines added by compinstall

########################################################################
# Oh-My-ZSH
########################################################################

export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=$HOME/.oh-my-zsh
export LS_COLORS="$(echo "$LS_COLORS" | sed -E 's/:mi=[0-9;]+/:mi=01;37/')"
ZSH_THEME="agnoster"
COMPLETION_WAITING_DOTS="true"
CASE_SENSITIVE="false"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
    git
    git-extras
    git-auto-fetch
    emacs
    vagrant
    docker
    dnf
    fzf
    fasd
    colorize
)
source $ZSH/oh-my-zsh.sh

# Adjust agnoster theme
SEGMENT_SEPARATOR=""
PRIMARY_FG=black
prompt_status() {
  local symbols
  symbols=()
  [[ $RETVAL -ne 0 ]] && symbols+="%{%F{red}%}$RETVAL"
  [[ $UID -eq 0 ]] && symbols+="%{%F{yellow}%}!!"
  [[ $(jobs -l | wc -l) -gt 0 ]] && symbols+="%{%F{cyan}%}&"

  [[ -n "$symbols" ]] && prompt_segment "$PRIMARY_FG" default "$symbols"
}
prompt_end() {
  if [[ -n $CURRENT_BG ]]; then
    print -n "%{%k%K{$CURRENT_BG}%}$SEGMENT_SEPARATOR%{%f%}\n"
  else
    print -n "%{%k%}"
  fi;
  print -n "#%{%f%k%}"
  CURRENT_BG=''
}

########################################################################

# extend loadpath
fpath=( ~/.zsh.d "${fpath[@]}" )

F=~/.zprofile
[ -f "$F" ] && source "$F"

unsetopt beep notify incappendhistory sharehistory
setopt appendhistory autocd extendedglob nomatch

export EDITOR=vim

alias vim=nvim

alias al='alias | grep'
alias g='grep'
cdg() {
	local maybe_path="$(git rev-parse --show-toplevel)"
	[ $? -eq 0 ] && cd "$maybe_path"
}
alias la='ll -a'
alias lt='ll -tr'
alias d='colordiff -u'
alias sctl='systemctl --user'
alias jctl='journalctl --user'

DOCKERNAME=docker
dbuild() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	$DOCKERNAME build -t "$NAME" .
}
drun() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	$DOCKERNAME run -ti --rm=true "$NAME" "${@}"
}
dbr() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	dbuild "$NAME" . && drun "$NAME" "${@}"
}
dbrp() {
	set -x;	for x in NAME PORT; do local "${x:-}"="${1:-}"; shift; done
	dbuild "$NAME" . && drun -p "$PORT" "$NAME" "${@}"
}
dbrp.() {
	set -x;	for x in PORT; do local "${x:-}"="${1:-}"; shift; done
	dbrp "$(pwd | xargs basename)" "$PORT" "${@}"
}
dbr.() {
	set -x
	dbr "$(pwd | xargs basename)" "${@}"
}
dexec() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	$DOCKERNAME exec -ti "$NAME" "${@}"
}
dshell() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	if [ -n "$NAME" ]; then
		dexec "$NAME" /bin/bash
	else
		dexec "$(docker ps -l --format '{{.ID}}')" /bin/bash
	fi
}
alias dp='$DOCKERNAME ps'
alias dpa='$DOCKERNAME ps --all'
alias tiga='tig --all'

alias vs='vagrant ssh'
alias vst='vagrant status'
alias vgs='vagrant global-status --prune'
alias vup='vagrant up'
alias vpa='vagrant provision --provision-with=ansible'

gdcommits() {
	d <(git show "$1") <(git show "$2")
}

# source zsh-syntax-highlighter must be the last line:
F=/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f "$F" ] && source "$F"
