#
# zsh/setopt.zsh
#

## Basics
setopt NO_BEEP
setopt AUTO_CD
# don't push multiple copies of the same directory into the directory stack
setopt PUSHD_IGNORE_DUPS
# treat #, ~, and ^ as part of patterns for filename generation
setopt EXTENDED_GLOB
# allow comments even in interactive shells (especially for Muness)
setopt INTERACTIVE_COMMENTS

## History
# allow multiple terminal sessions to all append to one zsh command history
setopt APPEND_HISTORY
# include timestamp of command and duration to history
setopt EXTENDED_HISTORY
# add comamnds as they are typed, don't wait until shell exit
setopt INC_APPEND_HISTORY
# do not write events to history that are duplicates of previous events
setopt HIST_IGNORE_DUPS
# remove command line from history list when it begins a space
setopt HIST_IGNORE_SPACE
# when searching history don't display results already cycled through twice
setopt HIST_FIND_NO_DUPS
# remove extra blanks from each command line being added to history
setopt HIST_REDUCE_BLANKS
# don't execute, just expand history
setopt HIST_VERIFY


## Completion
# allow completion from within a word/phrase
setopt COMPLETE_IN_WORD
# when completing from middle of a word, move cursor to the end of the word
setopt ALWAYS_TO_END
# show completion menu on successive tab press (needs 'unsetopt MENU_COMPLETE')
setopt AUTO_MENU
unsetopt MENU_COMPLETE

## Correction
# spelling correction for commands
setopt CORRECT
# spelling correction for arguments
#setopt CORRECTALL

## Prompt
# enable parameter expansion, command substitution, and arithmetic expansion
# in the prompt
setopt PROMPT_SUBST
# remove any right prompt from display when accepting a command line
setopt TRANSIENT_RPROMPT

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
