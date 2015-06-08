#! /bin/sh

## File:      supermake.sh
## Author(s): Michael Kifer, Baoqiu Cui
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1999
## 
## XSB is free software; you can redistribute it and/or modify it under the
## terms of the GNU Library General Public License as published by the Free
## Software Foundation; either version 2 of the License, or (at your option)
## any later version.
## 
## XSB is distributed in the hope that it will be useful, but WITHOUT ANY
## WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
## FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
## more details.
## 
## You should have received a copy of the GNU Library General Public License
## along with XSB; if not, write to the Free Software Foundation,
## Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
##
## $Id: supermake.sh,v 1.2 1999-06-22 17:56:36 cbaoqiu Exp $
## 
##

# This script helps XSB developers automatically configure and make XSB
# in all possible modes.  It is best to run this script after the local
# working directory has been cvs-updated.

# supermake.sh uses the same config-tags as those used in supertest.sh.
# $1 must be the path to the XSB installation directory.

xsbdir=$1

if test -z "$xsbdir" ; then
   echo "Arg 1 must be a path to the XSB installation directory"
   exit 1
elif test ! -d "$xsbdir" ; then
   echo "$xsbdir: XSB root directory doesn't exist or has wrong permissions"
   exit 1
fi

cd $xsbdir/build

echo "Configuring XSB with default options"
configure
echo "Making XSB with default options"
makexsb fast

echo "Configuring XSB with --enable-local-scheduling"
configure --config-tag=localsched --enable-local-scheduling
echo "Making XSB with --enable-local-scheduling"
makexsb --config-tag=localsched fast

echo "Configuring XSB with --enable-slg-wam"
configure --config-tag=slg-wam --enable-slg-wam
echo "Making XSB with --enable-slg-wam"
makexsb --config-tag=slg-wam fast

echo "Configuring XSB with --enable-slg-wam --enable-local-scheduling"
configure --config-tag=localschedNslg-wam --enable-slg-wam --enable-local-scheduling
echo "Making XSB with --enable-slg-wam --enable-local-scheduling"
makexsb --config-tag=localschedNslg-wam fast
