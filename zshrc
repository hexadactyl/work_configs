[[ $- != *i* ]] && return # if not interactive exit

export XDG_CONFIG_HOME="/home/devel/.config"

autoload -U compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## history
#setopt APPEND_HISTORY
## for sharing history between zsh processes
#setopt INC_APPEND_HISTORY
#setopt SHARE_HISTORY

## never ever beep ever
setopt NO_BEEP

autoload -U colors && colors

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
# Lines configured by zsh-newuser-install
HISTFILE=~/.config/zsh/.histfile
HISTSIZE=1000
SAVEHIST=5000
setopt appendhistory extendedglob nomatch
unsetopt autocd beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/devel/.zshrc'

autoload -Uz compinit
#zstyle ':completion:*' menu select
setopt COMPLETE_ALIASES
zstyle ':completion::complete:*' gain-privileges 1
compinit
# End of lines added by compinstall
autoload -U colors && colors
autoload -Uz promptinit && promptinit

# use vi mode
bindkey -v
export KEYTIMEOUT=1
# Fix backspace bug when switching modes
bindkey "^?" backward-delete-char

# syntax highlightign
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# set up autosuggestions
if [ ! -d $XDG_CONFIG_HOME/zsh/plugins/zsh-autosuggestions ]; then
	git clone https://github.com/zsh-users/zsh-autosuggestions $XDG_CONFIG_HOME/zsh/plugins/zsh-autosuggestions
fi

source $XDG_CONFIG_HOME/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# set up spaceship prompt
if [ ! -f /usr/local/share/zsh/site-functions/prompt_spaceship_setup ]; then
	git clone https://github.com/denysdovhan/spaceship-prompt.git --depth=1 $XDG_CONFIG_HOME/zsh/plugins/spaceship-prompt
	sudo mkdir -p /usr/local/share/zsh/site-functions/
	sudo chown -R devel:devel /usr/local/share/zsh/site-functions/
	ln -sf $XDG_CONFIG_HOME/zsh/plugins/spaceship-prompt/spaceship.zsh /usr/local/share/zsh/site-functions/prompt_spaceship_setup
fi

# spaceship configuration
SPACESHIP_PROMPT_ADD_NEWLINE=true
SPACESHIP_PROMPT_SEPARATE_LINE=true
SPACESHIP_CHAR_SYMBOL=❯
SPACESHIP_CHAR_SUFFIX=" "
SPACESHIP_HG_SHOW=false
SPACESHIP_PACKAGE_SHOW=false
SPACESHIP_NODE_SHOW=false
SPACESHIP_RUBY_SHOW=false
SPACESHIP_ELM_SHOW=false
SPACESHIP_ELIXIR_SHOW=false
SPACESHIP_XCODE_SHOW_LOCAL=false
SPACESHIP_SWIFT_SHOW_LOCAL=false
SPACESHIP_GOLANG_SHOW=false
SPACESHIP_PHP_SHOW=false
SPACESHIP_RUST_SHOW=false
SPACESHIP_JULIA_SHOW=false
SPACESHIP_DOCKER_SHOW=false
SPACESHIP_DOCKER_CONTEXT_SHOW=false
SPACESHIP_AWS_SHOW=false
SPACESHIP_CONDA_SHOW=false
SPACESHIP_VENV_SHOW=false
SPACESHIP_PYENV_SHOW=false
SPACESHIP_DOTNET_SHOW=false
SPACESHIP_EMBER_SHOW=false
SPACESHIP_KUBECONTEXT_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false
SPACESHIP_TERRAFORM_SHOW=false
SPACESHIP_VI_MODE_SHOW=false
SPACESHIP_JOBS_SHOW=false

autoload -U promptinit; promptinit
prompt spaceship

# other stuff
alias vim="nvim"
alias tesh="bossh.sh"

if command -v tmux >/dev/null 2>&1 && [ "${DISPLAY}" ]; then
    # if not inside a tmux session, and if no session is started, start a new session
    [ -z "${TMUX}" ] && (tmux attach || tmux) >/dev/null 2>&1
fi
