#!/usr/bin/bash

layer=$1
emacs_dir=$HOME/.emacs.d/layers
layer_dir=$emacs_dir/*/$layer

write_readme() {
    local layer_dir=$1
    local layer=$(basename $layer_dir)
    local orgfile=$layer.org

    cat $layer_dir/README.org | sed 's/#+TITLE:/*/' |  sed '/file:img/d' >> $orgfile
}

write_files() {
    local layer_dir=$1
    local layer=$(basename $layer_dir)
    local orgfile=$layer.org

    for file in $layer_dir/*.el; do
        local name=$(basename $file)
        local header="* $name"
        local begin_src="#+BEGIN_SRC emacs-lisp"
        local end_src="#+END_SRC"

        if [ -f $file ]; then
            echo "$header" >> $orgfile
            echo "$begin_src" >> $orgfile
            cat $file >> $orgfile
            echo "$end_src" >> $orgfile
        fi
    done
}

echo '' > $layer.org
write_readme $layer_dir
write_files $layer_dir
