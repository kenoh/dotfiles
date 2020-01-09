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

export PATH=$HOME/bin:$HOME/.local/bin:/usr/local/bin:$PATH
export ZSH=$HOME/.oh-my-zsh
export LS_COLORS="$(echo "$LS_COLORS" | sed -E 's/:mi=[0-9;]+/:mi=01;37/')"
ZSH_THEME=""
COMPLETION_WAITING_DOTS="true"
CASE_SENSITIVE="false"
HIST_STAMPS="yyyy-mm-dd"
plugins=(
    git
    git-extras
    vagrant
    docker
    dnf
    fzf
    fasd
    colorize
)
source $ZSH/oh-my-zsh.sh

########################################################################

# extend loadpath
fpath=( ~/.zsh.d "${fpath[@]}" )

# this for https://github.com/sindresorhus/pure
fpath+=( "$HOME/.zsh.d/pure" )
autoload -U promptinit; promptinit; prompt pure

F=~/.zprofile
[ -f "$F" ] && source "$F"

unsetopt beep notify incappendhistory sharehistory
setopt appendhistory autocd extendedglob nomatch

export EDITOR=vim

# aliases
alias zshrc-reload='. ~/.zshrc'

alias vim=nvim

test -n "$TILIX_ID" && export EMACSTERM=(env TERM=xterm-24bit)
alias e='emacsclient --no-wait'
alias et='$EMACSTERM emacsclient -t'
etdiff() { $EMACSTERM emacsclient -t --eval "(ediff-files \"$1\" \"$2\")"; }

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

DOCKERNAME=podman
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

alias openssl-cert-print-ascii='openssl x509 -text -noout -in'

# source zsh-syntax-highlighter must be the last line:
F=/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f "$F" ] && source "$F"

# kaychain
which keychain && keychain id_rsa
[ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`
[ -f $HOME/.keychain/$HOSTNAME-sh ] && \
		. $HOME/.keychain/$HOSTNAME-sh
[ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && \
		. $HOME/.keychain/$HOSTNAME-sh-gpg