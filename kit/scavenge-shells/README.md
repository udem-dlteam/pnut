# Scavenging Shells

This directory contains scripts to scavenge prebuilt shell executables from
historical Linux distributions, to be used in the root filesystem created
by`setup-rootfs.sh`.

To avoid having to redownload the ISO every time, the scripts in this directory
will extract the relevant files from the ISO and place them in this directory
in the form of a .tar file containing the install script and the relevant .deb
files. The install script will be used by `setup-rootfs.sh` to install the shell
in the root filesystem.

## bash-i386-woody

This script extracts bash 2.05a from Debian Woody (released in 2002). This is
the oldest version of bash that we can easily obtain that is compatible with
pnut.

## dash-i386-lenny

These scripts extract dash-0.5.4 from Debian Lenny (released in 2009). Only
version 0.5.5 of dash and later are compatible with pnut, since they include
support for arithmetic expansions. However, support for arithmetic expansions
was backported to the 0.5.4 version in Debian Lenny, so this version is also
compatible with pnut.

## dash-i386-squeeze

These scripts extract dash-0.5.5 from Debian Squeeze (released in 2011). This is
the first official version of dash that is compatible with pnut, since it
includes full support for arithmetic expansions.

## ksh-i386-sarge

This script extracts ksh_93q-2 from Debian Sarge (released in 2005).

## zsh-i386-sarge

This script extracts zsh 4.2.5 from Debian Sarge (released in 2005). This is the
first version of zsh that is compatible with pnut, including the printf builtin
which is required for pnut.
