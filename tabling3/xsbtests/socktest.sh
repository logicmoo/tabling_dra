#! /bin/sh

## File:      socktest.sh  - script for testing sockets
## Author(s): Michael Kifer
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
## $Id: socktest.sh,v 1.2 1999-11-30 04:35:06 kifer Exp $
## 
##

# THIS SCRIPT EXPECTS TO BE RUN FROM THE TESTSUITE DIRECTORY



while test 1=1
do
    case "$1" in
     *-opt*)
	    shift
	    options="-opts $1"
	    shift
	    ;;
	    
     *-tag*)
	    shift
	    config_opt="-tag $1"
	    shift
	    ;;

      *-mswin*)
	    shift
	    windows_opt=-mswin
	    echo "Running tests under Windows"
		;;

      *)
	    break
	    ;;
    esac
done


# install dir argument
if test -z "$1" -o $# -gt 1; then
    echo "Usage: socktest.sh [-opts opts] [-tag tag] [-mswin] <path>"
    echo "where: opts      -- options to pass to XSB"
    echo "       tag       -- the configuration tag to use"
    echo "       path      -- full path name of the XSB installation directory"
    echo ""
    exit
fi

installdir=$1

testsuite.sh -only socket_tests  $config_opt $options \
	    $windows_opt $installdir
