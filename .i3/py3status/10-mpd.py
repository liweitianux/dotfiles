#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/14

"""
mpd module for py3status
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
# regex to match the status string of 'mpc status' output
mpd_status_p = re.compile(r'.*\n^\[([a-z]+)\]', re.M)   # multiline

class Py3status:
    """
    MPD module to show mpd status and play info.

    This module also handle click events:
        left button:   mpc toggle
        middle button: mpc stop
        right button:  mpc next
    """

    def mpd(self, i3status_output_json, i3status_config):
        """
        Display MPD status and song info.
        """
        response = {
            'full_text': '',
            'name': 'mpd',
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
            cmd = 'mpc toggle'
            cmd_output = subprocess.getoutput(cmd)
        elif event['button'] == 2:
            # middle button click
            cmd = 'mpc stop'
            cmd_output = subprocess.getoutput(cmd)
        elif event['button'] == 3:
            # right button click
            cmd = 'mpc next'
            cmd_output = subprocess.getoutput(cmd)
        #
        self._update_response(i3status_config)
        os.system('killall -USR1 py3status')

    def _update_response(self, i3status_config=None):
        ## get mpd status
        cmd = 'mpc status'
        cmd_out = subprocess.getstatusoutput(cmd)
        if cmd_out[0] == 0:
            # mpd is running
            mpd_status_m = mpd_status_p.match(cmd_out[1])
            if mpd_status_m:
                self.status = mpd_status_m.group(1)
            else:
                self.status = 'stopped'
        else:
            # mpd not running (N/A)
            self.status = 'na'
        ## set full_text and color
        prompt = ''  # Icons: uF198 (music)
        if self.status == 'playing':
            status_text = ' '  # Icons: uF04B (play)
            cmd = 'mpc status -f "%artist%-%title%" | head -n 1'
            song_text = subprocess.getoutput(cmd)
            if i3status_config:
                self.color = i3status_config['color_good']
        elif self.status == 'paused':
            status_text = ' '  # Icons: uF04C (pause)
            cmd = 'mpc status -f "%title%" | head -n 1'
            song_text = subprocess.getoutput(cmd)
            if i3status_config:
                self.color = i3status_config['color_degraded']
        elif self.status == 'stopped':
            status_text = ''  # Icons: uF04D (stop)
            song_text = ''
            if i3status_config:
                self.color = i3status_config['color_bad']
        else:
            # mpd not running (N/A)
            status_text = ''  # Icons: uF00D (X)
            song_text = ''
            if i3status_config:
                self.color = i3status_config['color_bad']
        # full_text
        self.full_text = '{0} {1}{2}'.format(prompt, status_text, song_text)

