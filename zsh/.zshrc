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

WANNA_PATH="$HOME/bin:$HOME/.local/bin:/usr/local/bin"
case ":$PATH:" in
    *:"$WANNA_PATH":*)
	;;
    *)
	export PATH="$WANNA_PATH:$PATH"
esac
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
    fancy-ctrl-z
    z
)
source $ZSH/oh-my-zsh.sh

########################################################################

maybe() {
    which "$1" 1>/dev/null 2>&1
}

# extend loadpath
fpath=( ~/.zsh.d "${fpath[@]}" )

# this for https://github.com/sindresorhus/pure
fpath+=( "$HOME/.zsh.d/pure" )
autoload -U promptinit; promptinit; prompt pure

F=~/.zprofile
[ -f "$F" ] && source "$F"

unset command_not_found_handler

unsetopt beep notify incappendhistory sharehistory
setopt appendhistory autocd extendedglob nomatch


# history magic
setopt incappendhistory sharehistory

# local and global history at fingertips
## local
bindkey "^[OA" up-line-or-local-history
bindkey "^P" up-line-or-local-history
bindkey "^[OB" down-line-or-local-history
bindkey "^N" down-line-or-local-history
## global
bindkey "^[[1;5A" up-line-or-search
bindkey "^[[1;5B" down-line-or-search

up-line-or-local-history() {
    zle set-local-history 1
    zle up-line-or-history
    zle set-local-history 0
}
zle -N up-line-or-local-history
down-line-or-local-history() {
    zle set-local-history 1
    zle down-line-or-history
    zle set-local-history 0
}
zle -N down-line-or-local-history


# colors
export TERM=xterm-256color


# my editor
export EDITOR=vim


# aliases
alias zshrc-reload='. ~/.zshrc'

alias vim=nvim

alias e='emacsclient --no-wait'
alias et='emacsclient -t'
etdiff() { emacsclient -t --eval "(ediff-files \"$1\" \"$2\")"; }

alias al='alias | grep'
alias g='grep'
alias gi='grep -i'
cdg() {
	local maybe_path="$(git rev-parse --show-toplevel)"
	[ $? -eq 0 ] && cd "$maybe_path"
}
alias la='ll -a'
alias lt='ll -tr'
which colordiff 1>/dev/null 2>&1 \
    && alias d='colordiff -u' \
    || alias d='diff -u'
alias sctl='systemctl --user'
alias jctl='journalctl --user'

# docker/podman
alias dlastid='docker ps -l -q'
alias dbuild.='docker build -t k-$(basename "$PWD") .'
alias drunp='docker run -ti'
alias drunr='docker run -ti --rm=true'
alias dexec='docker exec -ti'
dshell() {
    LASTID="$(docker ps -l --format '{{.ID}}')"
    ID="${1:-$LASTID}"
    echo "$ID"
    docker exec -ti "$ID" /bin/bash
}
alias dp='docker ps'
alias dpa='docker ps --all'
dinspect() {
    docker inspect "$1" | jq
}
alias dip="docker inspect --format '{{ .NetworkSettings.IPAddress }}'"
alias dip,="docker inspect --format '{{ .NetworkSettings.IPAddress }}' $(dlastid)"

# git
alias gbvv='git -P branch -vv'
alias tiga='tig --all'
gdcommits() {
	d <(git show "$1") <(git show "$2")
}


# vagrant
alias vs='vagrant ssh'
alias vst='vagrant status'
alias vgs='vagrant global-status --prune'
alias vup='vagrant up'
alias vpa='vagrant provision --provision-with=ansible'

alias openssl-cert-print-ascii='openssl x509 -text -noout -in'

# virtualenvwrapper
maybe virtualenvwrapper.sh && source virtualenvwrapper.sh

# source zsh-syntax-highlighter must be the last line:
F=/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f "$F" ] && source "$F"


# kaychain
maybe keychain && keychain id_rsa
[ -z "$HOSTNAME" ] && HOSTNAME=`uname -n`
[ -f $HOME/.keychain/$HOSTNAME-sh ] && \
		. $HOME/.keychain/$HOSTNAME-sh
[ -f $HOME/.keychain/$HOSTNAME-sh-gpg ] && \
		. $HOME/.keychain/$HOSTNAME-sh-gpg
