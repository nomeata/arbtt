arbtt, the Automatic Rule-Based Time Tracker
============================================

© 2009 Joachim Breitner <mail@joachim-breitner.de>

The Automatic Rule-Based Time Tracker is a desktop daemon that runs in the
background and, every minute, records what windows are open on your
desktop, what their titles are, which one is active. The accompanied
statistics program lets you derive information from this log file, i.e.
what how much of your time have you been spending with e-Mail, or what
projects are your largest time wasters. The mapping from the raw window
titles to sensible „tags“ is done by a configuration file with an powerful
syntax.

Installation
------------

You can build and install this program as any other Cabalized program:

    $ runhaskell Setup.hs configure
    $ runhaskell Setup.hs build
    $ runhaskell Setup.hs install

You also need to make sure that arbtt-capture is started with your X
session:
- If you use GNOME or KDE, you can copy the file
  `arbtt-capture.desktop` to `~/.config/autostart/`. You might need to put the
  full path to arbtt-capture in the `Exec` line there, if you did not do a
  system wide installation.
- If you use macOS, you can use `launchd` for this.
  Create a .plist file like the following
  (with the path changed to match where arbtt-capture is located in your system):
  
  ```xml
  <?xml version="1.0" encoding="UTF-8"?>
  <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
  <plist version="1.0">
      <dict>
          <key>Label</key>
          <string>com.foo.arbtt</string>
          <key>Program</key>
          <string>/path/to/arbtt-capture</string>
          <key>RunAtLoad</key>
          <true/>
          <key>KeepAlive</key>
          <true/>
      </dict>
  </plist>
  ```
  and place it in `~/Library/LaunchAgents/com.foo.arbtt.plist`.
  You can replace "foo" with anything, such as your username, in both file name and content.
  This will ensure `arbtt-capture` is started whenever you log in.

If you want to record samples at a different rate than one per minute, you
will have to pass the `--sample-rate` parameter to arbtt-capture.

Documentation
------------

Full documentation is now provided in the user manual in the doc/
directory. If you have the docbook xsl toolchain installed, you can
generate the HTML documentation by entering "make" in that directory.
Otherwise, you can use the
[online version of the User’s Guide](http://arbtt.nomeata.de/doc/users_guide/index.html)
Beware that this will also reflect the latest development version.

Development
-----------

You are very welcome to help the developement of arbtt. You can find the
latest source at the git repository at
<https://github.com/nomeata/arbtt> or
<https://bitbucket.org/nomeata/arbtt>.

User and Developer discussion happens on the arbtt mailing list:
  arbtt@lists.nomeata.de
To subscribe to the list, visit:
  http://lists.nomeata.de/mailman/listinfo/arbtt

The issue tracker is hosted on bitbucket:
  <https://bitbucket.org/nomeata/arbtt/issues>
Why Bitbucket and not GitHub? Why not, and we need diversitiy even in the
cloud! (Don’t worry, you can use your GitHub account there.)

Some of my plans or ideas include:

 * A graphical viewer that allows you to expore the tags in an appealing,
   interactive way. Possibly based on the Charts haskell library.
 * Looking forward and backwards in time when writing rules. (Information
   is already passed to the categorizing function, but not exposed to the
   syntax).
 * `$total_idle` time, which is the maximum idle time until it is reset. This
   would allow the user to catch the idle times more exactly.
 * Rules based on time of day, to create tags for worktime, weekend, late
   at night. (Partially done)
 * Storing the current timezone in the tags, for the prevoius entry to be
   more to be more useful.
 * Storing the hostname, in case a user has several. 
 * Statistics based on time, to visualize trends.
 * Possibly more data sources?

Any help cleaning, documenting or testing the current code is appreciated
as well.

Creating the Windows Installer
------------------------------

The file `setup.iss` contains an installer script for Inno Setup and can be used
to create the windows installer for arbtt. It can be used under wine. To build
arbtt under Windows, you need to install the Haskell Platform. Because the
Haskell Platform ships an older version of the w32api package from mingw, you
also need to download `w32api-3.14-mingw32-dev.tar.gz` and copy at least the files
`include/psapi.h` and `lib/libpsapi.a` over the files installed by the Haskell
Platform. For the `pcre-light` package, you need to install the `pcre` library.
Unless you run a German version of Windows, you’ll need to adjust the path to
the `pcre3.dll` file in `setup.iss`. Install `Inno Setup`. Create the documentation
(`make -C doc`) and configure arbtt with the `--with-ISCC-flag`:

    $ wine runhaskell Setup.hs configure --with-ISCC='C:\Programme\Inno Setup 5\ISCC.exe'

again adjusting the path if you do not have a German version of Windows. This
will put the version name into `setup.iss` and create the output file as
`dist/arbtt-setup-<version>.exe.`

Download links:

 * http://hackage.haskell.org/platform/2009.2.0.2/HaskellPlatform-2009.2.0.2-setup.exe
 * http://sourceforge.net/projects/mingw/files/MinGW%20API%20for%20MS-Windows/
 * http://gnuwin32.sourceforge.net/downlinks/pcre.php
 * http://www.jrsoftware.org/download.php/is-unicode.exe

