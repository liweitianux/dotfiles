#!/usr/bin/env python3
#
# Add/Modify 'X-Label' header for mutt.
# Label mails with mutt.
#
# Reference:
# [1] GTD (Getting Things Done) and Mutt
#     https://docwhat.org/gtd-and-mutt/
#
# muttrc settings:
# unignore X-Label:
# macro index,pager   x       '<enter-command>set my_oldeditor=$editor<enter><enter-command>set editor="~/.mutt/mutt-xlabel.py"<enter><edit><previous-undeleted><clear-flag>N<sync-mailbox><enter-command>set editor=$my_oldeditor<enter>' \
#         "edit X-Label"
# macro index         \Cx     "<limit>~y " \
#         "limit view to label"
#
# Weitian LI <liweitianux@gmail.com>
# 2015/02/06
#

import sys
import os
import email
import readline


# Settings
HISTFILE  = os.path.join(os.environ["HOME"], ".mutt/xlabel_history")
LABELFILE = os.path.join(os.environ["HOME"], ".mutt/xlabels")


class MyCompleter(object):
    """
    Comstom completer for readline.

    Reference:
    [1] autocomplete - How to code autocompletion in python?
        http://stackoverflow.com/a/7821956
    """
    def __init__(self, options):
        self.options = sorted(options)

    def complete(self, text, state):
        if state == 0:  # on first trigger, build possible matches
            if text:    # cache matches (entries that start with entered text)
                self.matches = [s for s in self.options
                                    if s and s.startswith(text)]
            else:       # no text entered, all matches possible
                self.matches = self.options[:]

        # return match indexed by state
        try:
            return self.matches[state]
        except IndexError:
            return None


def my_input(prompt, default=None, completer=None):
    if default is not None:
        def pre_input_hook():
            readline.insert_text(default)
            readline.redisplay()
        readline.set_pre_input_hook(pre_input_hook)
    # completer
    if completer:
        readline.set_completer(completer)
        readline.parse_and_bind('tab: complete')
    return input(prompt)


def load_labels(labelfile):
    """
    Load saved labels from given labelfile,
    return a list of labels.
    """
    try:
        with open(labelfile, 'r') as f:
            label_list = f.read().split()
            label_list = list(set(label_list))
    except FileNotFoundError:
        label_list = []
    return label_list


def update_labels(labelfile, label_list, new_label_list):
    """
    Save labels for later autocompletion.
    """
    labels = sorted(list(set(label_list).union(set(new_label_list))))
    with open(labelfile, 'w') as f:
        f.write(' '.join(labels))


def get_xlabel(message):
    """
    Get 'X-Label:' values from given Message object.
    """
    labels = message.get_all('X-Label')
    if labels:
        label_str = ' '.join(labels)
    else:
        label_str = ''
    # remove duplicates and sort
    label_list = sorted(list(set(label_str.split())))
    return label_list


def write_xlabel(message, old_label_list, new_label_list, outfile):
    """
    Update 'X-Label' value of given Message object;
    then write Message object to outfile.
    """
    # remove duplicates and sort
    new_label_list = sorted(list(set(new_label_list)))
    if set(old_label_list) != set(new_label_list):
        # delete original 'X-Label' header (all occurences)
        del message['X-Label']
        # add new 'X-Label' header
        message['X-Label'] = ' '.join(new_label_list)
        # write to outfile (Just OVERWRITE, OK??)
        fp_out = open(outfile, 'w')
        fp_out.write(message.as_string())
        fp_out.close()


def main():
    if len(sys.argv) != 2 and len(sys.argv) != 3:
        print("Usage: %s <mail> [ outmail ]" % sys.argv[0])
        sys.exit(1)

    inmail = sys.argv[1]
    if len(sys.argv) == 3:
        outmail = sys.argv[2]
    else:
        outmail = inmail

    if hasattr(readline, 'read_history_file'):
        try:
            readline.read_history_file(HISTFILE)
        except IOError:
            pass

    # get all recorded labels for readline autocompletion
    all_labels = load_labels(LABELFILE)
    completer = MyCompleter(all_labels)

    # open mail and create email.message.Message object
    msg = email.message_from_file(open(inmail, 'r'))
    # get original labels
    label_list = get_xlabel(msg)
    # get user provided labels
    new_label = my_input(prompt='X-Label: ', default=' '.join(label_list),
            completer=completer.complete)
    # write new labels to mail
    write_xlabel(msg, label_list, new_label.split(), outmail)
    # save readline history
    readline.write_history_file(HISTFILE)
    # save labels
    update_labels(LABELFILE, all_labels, new_label.split())


if "__main__" == __name__:
    main()

