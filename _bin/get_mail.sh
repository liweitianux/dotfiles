#!/bin/sh
#
# Run the command to get emails, and record its PID, which is used
# to kill the program before run it again.
# This is an workaround to solve the stuck issue with `offlineimap`.
#

# Command to get emails
GET_CMD="offlineimap -o -1"

# PID file
PID_FILE="${HOME}/.cache/get_mail.pid"

# Log file
LOG_FILE="${HOME}/.cache/get_mail.log"


# Kill the previous process at first.
# For `offlineimap`, it sometimes just stucks ...
if [ -e "${PID_FILE}" ]; then
    kill -SIGKILL `cat ${PID_FILE}`
    rm ${PID_FILE}
fi

if [ -e "${LOG_FILE}" ]; then
    mv ${LOG_FILE} ${LOG_FILE}.old
fi

${GET_CMD} > ${LOG_FILE} 2>&1 &
echo $! > ${PID_FILE}
