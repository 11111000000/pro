NPM_PREFIX="${HOME}/.local"

export PATH="$PATH:$NPM_PREFIX/bin"

export MANPATH="${MANPATH-$(manpath)}:$NPM_PREFIX/share/man"

eval `ssh-agent -s`

export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$ANDROID_HOME/tools/bin/:$ANDROID_HOME/tools/:$PATH
export PATH=$ANDROID_HOME/emulator/:$PATH
export PATH=$ANDROID_HOME/platform-tools/:$PATH

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

nvm use 16
