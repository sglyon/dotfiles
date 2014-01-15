#!/usr/bin/env zsh

if [[ "$OSTYPE" == darwin* ]]; then
    rm $HOME/.zshrc
    rm $HOME/.zshenv

    ln -s $HOME/dotfiles/zsh/zshrc $HOME/.zshrc
    ln -s $HOME/dotfiles/zsh/zshenv $HOME/.zshenv
fi
