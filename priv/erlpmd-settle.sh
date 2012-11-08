#!/usr/bin/bash
RET=1
while [ "$RET" -eq "1" ]
do
	epmd -names > /dev/null 2>&1
	RET=$?
	sleep 0.05
done
exit $RET
