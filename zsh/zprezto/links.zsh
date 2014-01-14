if [[ "$OSTYPE" == darwin* ]]; then
    rm $HOME/.zlogin
    rm $HOME/.zlogout
    rm $HOME/.zprofile
    rm $HOME/.zshenv
    rm $HOME/.zpreztorc

    ln -s ./zlogin $HOME/.zlogin
    ln -s ./zlogout $HOME/.zlogout
    ln -s ./zprofile $HOME/.zprofile
    ln -s ./zshenv $HOME/.zshenv
    ln -s ./zpreztorc $HOME/.zpreztorc
fi
