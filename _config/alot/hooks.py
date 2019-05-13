#
# ~/.config/alot/hooks.py
#
# Credits:
# * https://github.com/pazz/alot/wiki/Contrib-Hooks
#

import re

import alot


# Check for missing attachment before sending
#
"""
async def pre_envelope_send(ui, dbm, __):
    p = r'.*([Aa]ttach|附件|附图|已附|所附)'
    e = ui.current_buffer.envelope
    if re.match(p, e.body, re.DOTALL) and not e.attachments:
        msg = 'No attachments. Send anyway?'
        if not (await ui.choice(msg, select='yes')) == 'yes':
            raise Exception()
"""


# Save marked position in search buffer
#
def pre_buffer_open(ui, dbm, buf):
    current = ui.current_buffer
    if isinstance(current, alot.buffers.SearchBuffer):
        current.focused_thread = current.get_selected_thread()

def post_buffer_focus(ui, dbm, buf, success):
    if success and hasattr(buf, "focused_thread"):
        if buf.focused_thread:
            tid = buf.focused_thread.get_thread_id()
            for pos, tlw in enumerate(buf.threadlist.get_lines()):
                if tlw.get_thread().get_thread_id() == tid:
                    buf.body.set_focus(pos)
                    break
