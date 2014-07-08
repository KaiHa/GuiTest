GuiTest
=======

Small scripts for making and comparing screenshots. The main purpose of this
scripts is in helping with the regression test of VisPro/VisTwo applications.
But with a few modifications this will also work for testing other graphical
user interfaces.

The process is the following:

 1. Record the mouse events. (`xmacrorec2 | ConvertClicks > clicks.txt`)
 2. Replay the mouse events and make screenshots after each click. This
    screenshots will become your reference screenshots. (`screenshot.sh` or
    `Screenshot`)
 3. Edit the reference screenshots so that each pixel that is expected to
    change is transparent. For example if a clock is displayed in your
    _application under test_ make the area of the clock transparent.  `MaskImg`
    can help you by transferring the transparent areas of one image onto
    another image.
 4. Install the new version of your _application under test_.
 5. Replay the mouse events and make screenshots after each click.
    (`screenshot.sh` or `Screenshot`)
 6. Use `DiffImg` to compare these screenshots with your reference screenshots.
