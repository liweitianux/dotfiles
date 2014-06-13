#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/15

"""
touchpad module for py3status
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


POSITION = 0


class Py3status:
    """
    Touchpad module to show the status of touchpad.

    Click events:
        left button:   enable touchpad
        middle button: toggle touchpad
    """

    def __init__(self):
        """
        get touchpad status
        """
        cmd = 'synclient -l | grep -c "TouchpadOff.*=.*0"'
        touchpad_status = subprocess.getoutput(cmd)
        if int(touchpad_status) == 1:
            self.status = 'on'
        else:
            self.status = 'off'

    def touchpad(self, i3status_output_json, i3status_config):
        """
        Display touchpad status.
        """
        #prompt = ''  # Icons: uF10A (ipad)
        prompt = ''  # Icons: uF3E3 (palm)
        response = {
            'full_text': '',
            'name': 'touchpad',
            'instance': 'first',
        }
        if self.status == 'on':
            response['color'] = i3status_config['color_good']
            status_text = ''  # Icons: uF00C (check)
        else:
            response['color'] = i3status_config['color_bad']
            status_text = ''  # Icons: uF00D (cross)
        response['full_text'] = '{0} {1}'.format(prompt, status_text)
        return (POSITION, response)

    def on_click(self, i3status_output_json, i3status_config, event):
        """
        Handle click events.
        """
        if event['button'] == 1:
            # left button click
            cmd = 'synclient TouchpadOff=0'
            cmd_output = subprocess.getoutput(cmd)
            self.status = 'on'
        elif event['button'] == 2:
            # middle button click
            if self.status == 'on':
                cmd = 'synclient TouchpadOff=1'
                self.status = 'off'
            else:
                cmd = 'synclient TouchpadOff=0'
                self.status = 'on'
            cmd_output = subprocess.getoutput(cmd)
        #
        os.system('killall -USR1 py3status')

