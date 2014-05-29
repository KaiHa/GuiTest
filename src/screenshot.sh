#!/bin/bash

set -e

export DISPLAY=:1
screenshotcount=0

check_cmd()
{
    if ! which $1 > /dev/null
    then
        echo "error: Command '$1' not found! Please install '$2'."
        exit 2
    fi
}

check_cmd import imagemagick
check_cmd visrun vispro
check_cmd xte    xautomation
check_cmd Xvfb   xvfb

screenshot()
{
    ((screenshotcount++))
    import -window root actual/screenshot_${screenshotcount}.png
}

# click x-position y-position
click()
{
    xte <<-END
		mousemove $1 $2
		mouseclick 1
		sleep 1
	END
    screenshot
}

mkdir -p actual

# start virtual frambuffer
Xvfb :1 -screen 0 1280x1024x24 &> /dev/null &
sleep 1
visrun -alaheight 0 -geom 1280x1024 &
sleep 4


screenshot

. clicks.txt

kill %2
kill %1

echo DONE
