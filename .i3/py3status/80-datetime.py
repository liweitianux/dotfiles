#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Weitian LI <liweitianux@gmail.com>
# 2014/05/15

"""
datetime module for py3status
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


# regext to match date and time from output of 'date'
datetime_p = re.compile(r'(^\d+/\d+)\s*(\d+:\d+)')


class Py3status:
    """
    Datetime module to show current date and time.
    """

    def mydate(self, i3status_output_json, i3status_config):
        """
        Display current date.
        """
        POSITION = 0
        response = {
            'full_text': '',
            'name': 'mydate',
            'instance': 'first',
        }
        prompt = ''  # Icons: uF073 (calendar)
        # update datetime
        self._get_datetime()
        response['full_text'] = '{0} {1}'.format(prompt, self.date)
        # cache status for 5 seconds
        response['cached_until'] = time.time() + 5
        return (POSITION, response)

    def mytime(self, i3status_output_json, i3status_config):
        """
        Display current time.
        """
        POSITION = 1
        response = {
            'full_text': '',
            'name': 'mytime',
            'instance': 'first',
        }
        prompt = ''  # Icons: uF017 (clock)
        # update datetime
        self._get_datetime()
        response['full_text'] = '{0} {1}'.format(prompt, self.time)
        # cache status for 5 seconds
        response['cached_until'] = time.time() + 5
        return (POSITION, response)

    def _get_datetime(self):
        """
        Get current date and time
        """
        cmd = 'date "+%m/%d %H:%M"'
        datetime = subprocess.getoutput(cmd)
        dt_m = datetime_p.match(datetime)
        self.date, self.time = dt_m.group(1, 2)

