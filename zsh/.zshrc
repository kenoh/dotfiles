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
    fancy-ctrl-z
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

bindkey "^[[A" up-line-or-local-history
bindkey "^P" up-line-or-local-history
bindkey "^[[B" down-line-or-local-history
bindkey "^N" down-line-or-local-history

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


# my editor
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
DOCKERNAME=docker
dbuild() { $DOCKERNAME build -t "$(basename "$PWD")" "${@}"; }
dbuild.() { dbuild "${@}" .; }
drun() { $DOCKERNAME run -ti --rm=true "${@}"; }
drun.() { drun "$(basename "$PWD")" "${@}"; }
dexec() { $DOCKERNAME exec -ti "${@}"; }
dshell() { $DOCKERNAME exec "$(docker ps -l --format '{{.ID}}')" /bin/bash; }
alias dp='$DOCKERNAME ps'
alias dpa='$DOCKERNAME ps --all'

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
