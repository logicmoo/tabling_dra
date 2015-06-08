/* File:      cvarstring.c
** Author(s): Swift
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** 
** XSB is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** XSB is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with XSB; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** $Id: cvarconstr.c,v 1.1 2008-04-07 00:27:19 tswift Exp $
** 
*/

/*   Simple example file showing how to call XSB from C without varstrings  
 *   To make this file, see the instructions in ./README */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* cinterf.h is necessary for the XSB API, as well as the path manipulation routines*/
#include "cinterf.h"
extern char *xsb_executable_full_path(char *);
extern char *strip_names_from_path(char*, int);

/* context.h is necessary for the type of a thread context. */
#include "context.h"

int main(int argc, char *argv[])
{ 
  int rc;

  /* xsb_init_string relies on the calling program to pass the absolute or relative
     path name of the XSB installation directory. We assume that the current
     program is sitting in the directory ../examples/c_calling_xsb/
     To get installation directory, we strip 3 file names from the path. */

  int myargc = 1;
  char *myargv[1];
#ifdef WIN_NT
    VarString return_string, *rs;
    XSB_StrCreate(&rs);
    XSB_StrInit(rs);
    return_string = *rs;
#else
    XSB_StrDefine(return_string);
#endif

  myargv[0] = argv[1];

  if (xsb_init(myargc,myargv)) {
    fprintf(stderr,"%s initializing XSB: %s\n",xsb_get_init_error_type(),xsb_get_init_error_message());
    exit(XSB_ERROR);
  }

#ifdef MULTI_THREAD
  th_context *th = xsb_get_main_thread();
#endif

  /* Create command to consult clprtest.P */
  if (xsb_command_string(CTXTc "consult('clprtest.P').") == XSB_ERROR)
    fprintf(stderr,"++Error consulting clprtest.P: %s/%s\n",xsb_get_error_type(CTXT),
	    xsb_get_error_message(CTXT));

  rc = xsb_query_string_string(CTXTc "test15(X,Y,Z).",&return_string,"|");
  //  printf(" rc %d\n",rc);
  while (rc == XSB_SUCCESS) {
    printf("cvsreturn %s\n",(return_string.string));
    rc = xsb_next_string(CTXTc &return_string,"|");
  }

 if (rc == XSB_ERROR) 
   fprintf(stderr,"++Query Error: %s/%s\n",xsb_get_error_type(CTXT),xsb_get_error_message(CTXT));

  xsb_close(CTXT);      /* Close connection */
  return(0);
}
