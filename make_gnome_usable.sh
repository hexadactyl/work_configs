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

sudo yum install -y gnome-tweaks
sudo yum install -y neovim
sudo yum install -y zsh
sudo yum install -y zsh-syntax-highlighting
sudo yum install -y kitty
cp /home/linuxbrew/.linuxbrew/var/homebrew/linked/fzf/shell/key-bindings.zsh $HOME/.config/zsh/

sudo yum-config-manager --add-repo=https://copr.fedorainfracloud.org/coprs/carlwgeorge/ripgrep/repo/epel-7/carlwgeorge-ripgrep-epel-7.repo
sudo yum install ripgrep -y

brew install fzf
$(brew --prefix)/opt/fzf/install
source ~/.config/zsh/.zshrc

echo "Remember to change the font to mononoki"
