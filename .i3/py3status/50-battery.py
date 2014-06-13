#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/17

"""
battery module for py3status
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

BAT_ID = 0
## uevent:
##   ENERGY_NOW: energy that battery has now
##   POWER_NOW:  battery current discharging/charing rate
BAT_UEVENT = '/sys/class/power_supply/BAT{0}/uevent'.format(BAT_ID)

# regex to match pattern in 'uevent' contents'
status_p    = re.compile(r'.*STATUS=(\w+)', re.S)
present_p   = re.compile(r'.*PRESENT=(\d)', re.S)
capacity_p  = re.compile(r'.*CAPACITY=(\d+)', re.S)
energynow_p = re.compile(r'.*ENERGY_NOW=(\d+)', re.S)
powernow_p  = re.compile(r'.*POWER_NOW=(\d+)', re.S)


class Py3status:
    """
    Battery module to show battery information.

    battery status: Full, Discharging, Charging, Unknown
    """

    def mybattery(self, i3status_output_json, i3status_config):
        """
        Display battery information.
        """
        response = {
            'full_text': '',
            'name': 'mybattery',
            'instance': 'first',
        }
        # open battery uevent file
        with open(BAT_UEVENT, 'r') as bat_f:
            bat_info = bat_f.read()
        status_m    = status_p.match(bat_info)
        capacity_m  = capacity_p.match(bat_info)
        energynow_m = energynow_p.match(bat_info)
        powernow_m  = powernow_p.match(bat_info)
        self.status   = status_m.group(1)
        self.capacity = int(capacity_m.group(1))
        energynow = int(energynow_m.group(1))
        powernow  = int(powernow_m.group(1))
        #
        bat_text = '{0}%'.format(self.capacity)
        if self.status == 'Full':
            prompt = ''  # Icons: uF213 (battery-full)
            color = i3status_config['color_good']
        elif self.status == 'Charging':
            prompt = ''  # Icons: uF211 (battery-charing)
            color = i3status_config['color_degraded']
        elif self.status == 'Discharging':
            if self.capacity > 80:
                prompt = ''  # Icons: uF213 (battery-full)
                color = i3status_config['color_good']
            elif self.capacity > 60:
                prompt = ''  # Icons: uF214 (battery-high)
                color = i3status_config['color_degraded']
            elif self.capacity > 30:
                prompt = ''  # Icons: uF215 (battery-low)
                color = i3status_config['color_degraded']
            else:
                prompt = ''  # Icons: uF212 (battery-empty)
                color = i3status_config['color_bad']
            # calc remaining time
            remaining_h = energynow // powernow
            remaining_m = int((energynow % powernow) / powernow * 60)
            if remaining_h:
                remaining = '{0}h{1:02d}m'.format(remaining_h, remaining_m)
            else:
                remaining = '{0:02d}m'.format(remaining_m)
            bat_text = '{0} {1}'.format(bat_text, remaining)
        else:
            # unknown battery status
            prompt = ''  # Icons: uF215 (battery-low)
            prompt = prompt + ' '  # Icons: uF243 (question-sign)
            color = i3status_config['color_bad']
        #
        response['full_text'] = '{0} {1}'.format(prompt, bat_text)
        response['color'] = color
        # cache status for 5 seconds
        response['cached_until'] = time.time() + 5
        return (POSITION, response)

