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
compinit
# End of lines added by compinstall

########################################################################
# Oh-My-ZSH
########################################################################

export PATH=$HOME/bin:/usr/local/bin:$PATH
export ZSH=$HOME/.oh-my-zsh
export LS_COLORS="$(echo "$LS_COLORS" | sed -E 's/:mi=[0-9;]+/:mi=01;37/')"
ZSH_THEME="lambda"
COMPLETION_WAITING_DOTS="true"
CASE_SENSITIVE="false"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
    git
    emacs
    vagrant
    docker
    fedora
    fzf
    fasd
)
source $ZSH/oh-my-zsh.sh

########################################################################

unsetopt beep notify incappendhistory sharehistory
setopt appendhistory autocd extendedglob nomatch

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
	set -x;	for x in DUMMY; do local "${x:-}"="${1:-}"; shift; done
	dbr "$(pwd | xargs basename)" "${@}"
}
dexec() {
	set -x;	for x in NAME; do local "${x:-}"="${1:-}"; shift; done
	$DOCKERNAME exec -ti "$NAME" "${@}"
}
alias dp='$DOCKERNAME ps'
alias dpa='$DOCKERNAME ps --all'
alias tiga='tig --all'

gdcommits() {
	d <(git show "$1") <(git show "$2")
}

# load scripts from a dir
[ -d ~/.zsh.d/ ] && fpath=(~/.zsh.d/ $fpath)

# source zsh-syntax-highlighter must be the last line:
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh