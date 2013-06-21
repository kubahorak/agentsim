#!/bin/bash
#
# Tests the reference agent by running multiple simulations in GUI.
# This script depends on xdotool <http://www.semicomplete.com/projects/xdotool/>
#
# Substitute XXX with your own environment name.
#
# Run the script from the project root directory like this:
# $ test/run-agent.sh
#

# number of test runs
NUMRUNS=20
# the time in seconds it takes the system to show the GUI
SLEEPTIME=20

function run {
	tmp=/tmp/run-agent-tmp
	./run.sh > $tmp <<EOF
(setf *random-state* (make-random-state T))
(run-gui (make-hs-world6-XXX))
(exit)
EOF
	# random state is printed on two lines, so join them into one
	randomstate=`grep --after 1 'RANDOM-STATE' $tmp | sed -e 'N;s/\n/ /'`
	grep '\*\*\*' $tmp
	if [ $? -eq 0 ]; then
		# match
		echo "ERROR: $randomstate"
	else
		echo "SUCCESS"
	fi
	rm $tmp
}

projectdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. && pwd )"

pushd "$projectdir" >/dev/null

for i in `seq 1 $NUMRUNS`; do
	run &
	sleep $SLEEPTIME; xdotool key t
	wait
done

popd >/dev/null
