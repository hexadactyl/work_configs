#!/bin/sh
gsettings set org.gnome.desktop.wm.preferences button-layout ":minimize,maximize,close"
#gsettings set org.gnome.desktop.background show-desktop-icons true

sudo zsh -c 'cat << EOF > /etc/zshenv
export XDG_CONFIG_HOME="\$HOME/.config"
export ZDOTDIR="\$XDG_CONFIG_HOME/zsh"
EOF'

mkdir $HOME/.config/zsh
mkdir $HOME/.config/nvim

sudo chsh -s /bin/zsh devel

sudo yum install gnome-tweaks -y
sudo yum install neovim -y
sudo yum install zsh -y
sudo yum install fira-code-fonts -y
sudo yum install zsh-syntax-highlighting -y

brew install fzf
$(brew --prefix)/opt/fzf/install
source ~/.config/zsh/.zshrc

echo "Remember to change the font to fira code!"
