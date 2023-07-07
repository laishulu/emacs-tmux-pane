#!/usr/bin/env bash

# File Based on
# https://github.com/christoomey/vim-tmux-navigator/blob/2fd76aa930e34838335c1aa125b88e7ea671e6c0/vim-tmux-navigator.tmux
version_pat='s/^tmux[^0-9]*([.0-9]+).*/\1/p'

is_vim_emacs='echo "#{pane_current_command}" | \
    grep -iqE "((^|\/)g?(view|n?vim?x?)(diff)?$)|emacs"'

# enable in root key table
tmux bind-key -n C-h if-shell "$is_vim_emacs" "send-keys C-h" "select-pane -L"
tmux bind-key -n C-j if-shell "$is_vim_emacs" "send-keys C-j" "select-pane -D"
tmux bind-key -n C-k if-shell "$is_vim_emacs" "send-keys C-k" "select-pane -U"
tmux bind-key -n C-l if-shell "$is_vim_emacs" "send-keys C-l" "select-pane -R"

tmux_version="$(tmux -V | sed -En "$version_pat")"
setenv -g tmux_version "$tmux_version"

#echo "{'version' : '${tmux_version}', 'sed_pat' : '${version_pat}' }" > ~/.tmux_version.json

tmux if-shell -b '[ "$(echo "$tmux_version < 3.0" | bc)" = 1 ]' \
  "bind-key -n 'C-\\' if-shell \"$is_vim_emacs\" 'send-keys C-\\'  'select-pane -l'"
tmux if-shell -b '[ "$(echo "$tmux_version >= 3.0" | bc)" = 1 ]' \
  "bind-key -n 'C-\\' if-shell \"$is_vim_emacs\" 'send-keys C-\\\\'  'select-pane -l'"

tmux bind-key -T copy-mode-vi 'C-h' if-shell "$is_vim_emacs" "send-keys C-h" "select-pane -L"
tmux bind-key -T copy-mode-vi 'C-j' if-shell "$is_vim_emacs" "send-keys C-j" "select-pane -D"
tmux bind-key -T copy-mode-vi 'C-k' if-shell "$is_vim_emacs" "send-keys C-k" "select-pane -U"
tmux bind-key -T copy-mode-vi 'C-l' if-shell "$is_vim_emacs" "send-keys C-l" "select-pane -R"
tmux bind-key -T copy-mode-vi 'C-\' if-shell "$is_vim_emacs" "send-keys C-\\\\" "select-pane -l"
