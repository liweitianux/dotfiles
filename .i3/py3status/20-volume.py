#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/15

"""
volume module for py3status
"""

## Get output of shell command:
## python 3.x:
## >>> subprocess.getoutput(cmd)
## >>> subprocess.getstatusoutput(cmd)
## python 2.7.x:
## >>> subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE)

import os
import re
import subprocess
import time


POSITION = 0
DEVICE = 'default'
MIXER = 'Master'
# regex compile to match amixer output volume info
amixer_rep = re.compile(r'.*\[(\d+)%\]\s*\[(on|off)\]')


class Py3status:
    """
    Volume module to show volume info.

    This module also handle click events:
        left button:   decrease volume
        middle button: mute/unmute toggle
        right button:  increase volume
    """

    def myvolume(self, i3status_output_json, i3status_config):
        """
        Display volume info.

        XXX: cannot use name 'volume' ??
        """
        response = {
            'full_text': '',
            'name': 'myvolume',
            'instance': 'first',
        }
        # update response fields
        self._update_response(i3status_config)
        if self.color:
            response['color'] = self.color
        response['full_text'] = self.full_text
        # cache status for 5 seconds
        response['cached_until'] = time.time() + 5
        return (POSITION, response)

    def on_click(self, i3status_output_json, i3status_config, event):
        """
        Handle click events.
        """
        if event['button'] == 1:
            # left button click
            cmd = 'amixer -D "{0}" sset "{1}" "5%-" unmute'.format(DEVICE, MIXER)
            cmd_output = subprocess.getoutput(cmd)
        elif event['button'] == 2:
            # middle button click
            cmd = 'amixer -D "{0}" sset "{1}" toggle'.format(DEVICE, MIXER)
            cmd_output = subprocess.getoutput(cmd)
        elif event['button'] == 3:
            # right button click
            cmd = 'amixer -D "{0}" sset "{1}" "5%+" unmute'.format(DEVICE, MIXER)
            cmd_output = subprocess.getoutput(cmd)
        #
        self._update_response(i3status_config)
        os.system('killall -USR1 py3status')

    def _update_response(self, i3status_config=None):
        """
        update self.full_text
        """
        # get volume info
        cmd = 'amixer -D "{0}" sget "{1}" | tail -n 1'.format(DEVICE, MIXER)
        cmd_output = subprocess.getoutput(cmd)
        m = amixer_rep.match(cmd_output)
        self.volume, self.status = m.group(1, 2)
        if int(self.volume) > 60:
            self.level = 'high'
            prompt = ''  # Icons: uF357 (volume-high)
            if i3status_config:
                self.color = i3status_config['color_degraded']
        elif int(self.volume) > 30:
            self.level = 'medium'
            prompt = ''  # Icons: uF359 (volume-medium)
            if i3status_config:
                self.color = i3status_config['color_good']
        else:
            self.level = 'low'
            prompt = ''  # Icons: uF358 (volume-low)
            if i3status_config:
                self.color = i3status_config['color_good']
        # determine icon
        if self.status == 'off':
            prompt = ''  # Icons: uF35A (volume-off)
            if i3status_config:
                self.color = i3status_config['color_bad']
        # determine display info
        self.full_text = '{0} {1}%'.format(prompt, self.volume)

