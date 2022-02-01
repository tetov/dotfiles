_lscolors_install_dir="$HOME/.local/share"
_lscolors_tmp_dir="/tmp/LS_COLORS"

if [[ ! -e "$_lscolors_install_dir/lscolors.sh" ]] ; then
  mkdir "$_lscolors_tmp_dir" && \
    curl -L https://api.github.com/repos/trapd00r/LS_COLORS/tarball/master | \
    tar xzf - --directory="$_lscolors_tmp_dir" --strip=1
  (cd $_lscolors_tmp_dir && sh ./install.sh)  # subshell this so cwd isn't changed
fi
source "$_lscolors_install_dir/lscolors.sh"

unset _lscolors_install_dir _lscolors_tmp_dir
