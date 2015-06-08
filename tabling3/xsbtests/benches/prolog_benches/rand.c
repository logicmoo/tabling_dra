#include <stdio.h>
#include <stdlib.h>

#include "cinterf.h"

int prand(void)
{
  unsigned int r = rand();

  r = r & 0xFFFFFF;
  ctop_int(1,r);
  return(1);
}

