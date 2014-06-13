#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/15

"""
power module for py3status
"""

## Get output of shell command:
## python 3.x:
## >>> subprocess.getoutput(cmd)
## >>> subprocess.getstatusoutput(cmd)
## python 2.7.x:
## >>> subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE)

import re
import subprocess
import time


POSITION = 0

BAT_PRESENT = '/sys/class/power_supply/BAT0/present'
# regex to match the '1' at the beginning
one_p = re.compile(r'(^1).*')


class Py3status:
    """
    Power module to show whether AC/battery is online.

    ac_status: online, offline
    bat_status: present, na
    """

    def power(self, i3status_output_json, i3status_config):
        """
        Display power status.
        """
        response = {
            'full_text': '',
            'name': 'power',
            'instance': 'first',
        }
        # AC online
        ac_cmd = 'on_ac_power'
        ac_cmd_out = subprocess.getstatusoutput(ac_cmd)
        if ac_cmd_out[0] == 0:
            ac_text = ''  # Icons: uF237 (ac-online)
            self.ac_status = 'online'
            response['color'] = i3status_config['color_good']
        else:
            ac_text = ''  # Icons: uF236 (ac-offline)
            self.ac_status = 'offline'
            response['color'] = i3status_config['color_bad']
        # Battery present
        with open(BAT_PRESENT, 'r') as bat_present_f:
            bat_present_c = bat_present_f.read()
        bat_m = one_p.match(bat_present_c)
        if bat_m.group(1):
            bat_text = ' '  # Icons: uF3CF (battery-vertical)
            self.bat_status = 'present'
        else:
            bat_text = ''
            self.bat_status = 'na'
        #
        response['full_text'] = '{0} {1}'.format(ac_text, bat_text)
        # cache status for 5 seconds
        response['cached_until'] = time.time() + 5
        return (POSITION, response)

