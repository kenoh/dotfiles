# -*- mode: sh; -*-


# DEBUG: profile (also uncomment the zprof at the bottom).
# zmodload zsh/zprof


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


_comp_options+=(globdots) # Complete also hidden files
zstyle ':completion:alias-expansion:*' completer _expand_alias

unsetopt beep notify
setopt incappendhistory appendhistory sharehistory \
	autocd extendedglob nomatch 


command_not_found_handle() { echo "I don't know what '$1' is." >&2; return 1; }


export EDITOR=vim


## Fix home/end keys
bindkey '\e[H' beginning-of-line
bindkey '\e[F' end-of-line 
bindkey  "^[[3~"  delete-char


## local and global history at fingertips
# local
bindkey "^[OA" up-line-or-local-history
bindkey "^P" up-line-or-local-history
bindkey "^[OB" down-line-or-local-history
bindkey "^N" down-line-or-local-history
# global
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


### Aliases
has() { which "$1" 1>/dev/null 2>&1; }

## cd
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'

## ls
alias l='ls'
alias ll='ls -l'
alias la='ls -la'
alias lt='ls -latr'

## grep
alias al='alias | grep'
alias g='grep'
alias gi='grep -i'

## diff
has colordiff \
    && alias d='colordiff -u' \
    || alias d='diff -u'
douts() { # usage: douts BEFORE FST SND [DIFF-OPTS [AFTER]]
	eval "${DIFFTOOL:-d} $4 <($1 $2 $5) <($1 $3 $5)"; }

## git
alias ga='git add'
alias gau='git add -u'
alias gb='git branch'
alias gbvv='git -P branch -vv'
alias gc='git commit -v'
alias gc!='git commit --amend -v'
alias gco='git checkout'
alias gd='git diff'
alias gdca='git diff --cached'
alias gdt='git difftool'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gl='git pull'
alias glg='git log'
alias glgp='git log -p'
alias gm='git merge'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias grb='git rebase'
alias gr='git remote'
alias grv='git remote -v'
alias gsh='git show'
alias gst='git status'
alias gss='git status -s'
alias gsta='git stash'
alias gstl='git stash list'
alias gstp='git stash pop'
alias tiga='tig --all'
gdcommits() {
	d <(git show "$1") <(git show "$2")
}
cdg() {
	local maybe_path="$(git rev-parse --show-toplevel)"
	[ $? -eq 0 ] && cd "$maybe_path"
}

## systemd
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
difname() {  # gives iface
    ip a | grep "^$(docker exec -i $1 cat /sys/class/net/eth0/iflink)";
}
dips() {  # list containers with their IPs
    docker ps \
	| awk 'NR>1{ print $1 }' \
	| xargs docker inspect \
	  -f '{{$.Name}}{{"\t"}}{{range $i, $e := .NetworkSettings.Networks}}{{print $i ": " $e.IPAddress ", "}}{{end}}' \
	| column --table -s $'\t' \
	| sort
}

## docker compose
,dcrmi() {  # remove image behind a docker compose container
	docker stop "${1}_1"
	docker rm "${1}_1"
	docker rmi "${1}"
}
alias dc='docker compose'


## emacs
alias e='emacsclient --no-wait -c'
alias et='emacsclient -t'
etdiff() { emacsclient -t --eval "(ediff-files \"$1\" \"$2\")"; }




###############################################################################
# feature rich bariere
[[ "$(hostname)" = "quincampoix" ]] || return 0
###############################################################################


local WANTED_PATH="$HOME/Android/Sdk/platform-tools:$HOME/go/bin:$HOME/bin:$HOME/.local/bin:/usr/local/bin"
[[ ":$PATH:" = *:$WANTED_PATH:* ]] || export PATH="$WANTED_PATH:$PATH"


## aliases
alias vim=nvim
alias xo=xdg-open


# kitty terminal emulator
if [ "x$TERM" = "xxterm-kitty" ]; then
    kitty + complete setup zsh | source /dev/stdin
    #alias d="kitty +kitten diff"
    alias ssh="TERM=xterm-256color ssh"
fi


## virtualenvwrapper
has virtualenvwrapper_lazy.sh && source virtualenvwrapper_lazy.sh


## nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" --no-use # This loads nvm, --no-use to make it more lazy-loaded.

alias node='unalias node ; unalias npm ; nvm use default ; node $@'
alias npm='unalias node ; unalias npm ; nvm use default ; npm $@'


## antidote
test -d ${ZDOTDIR:-~}/.antidote || git clone --depth=1 https://github.com/mattmc3/antidote.git ${ZDOTDIR:-~}/.antidote
source ${ZDOTDIR:-~}/.antidote/antidote.zsh
antidote load


## zsh-syntax-highlighter (must be basically the last in zshrc)
F=/usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
[ -f "$F" ] && source "$F"


# DEBUG: profile (also enable the modload at the top).
# zprof


## starship
eval $(starship init zsh)
