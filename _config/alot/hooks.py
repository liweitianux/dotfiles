#
# ~/.config/alot/hooks.py
#
# Credits:
# * https://github.com/pazz/alot/wiki/Contrib-Hooks
#

import re


# Check for missing attachment before sending
#
async def pre_envelope_send(ui, dbm, __):
    p = r'.*([Aa]ttach|附件|附图)'
    e = ui.current_buffer.envelope
    if re.match(p, e.body, re.DOTALL) and not e.attachments:
        msg = 'No attachments. Send anyway?'
        if not (await ui.choice(msg, select='yes')) == 'yes':
            raise Exception()
