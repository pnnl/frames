#ifndef CONSTS_H
#define CONSTS_H

#include <math.h>
#include <dir.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <values.h>
#include <except.h>
#include "..\Common files\fcsv.h"
#include "..\Common files\gid.h"
#include "error.h"


const int PeakEvent = 1;
const int ValleyEvent = 0;

const int MaxPoints = 8;
const int MaxName = 48;
const int MaxUnits = 48;
const int MaxLoc = 5;

extern int siteidx,modidx,numcon;
extern int numsrc,numlife,numlocs;


#endif
