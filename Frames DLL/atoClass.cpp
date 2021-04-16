/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "atoClass.h"

static ATO *ato = NULL;


char *POLAR = "Polar";
char *CARTESIAN = "Cartesian";

bool writelinear = false;
int g_x = 0;
int g_y = 0;
int g_z = 0;

// when loading grids, they will access this
// to obtain access to flux data to properly rename
// the flux if needed (ie if it conflicts with an
// existing flux already in the global flux list)
int globalfluxnum = 0;
ATOSet *currentDataset = NULL;
ATOFluxType **globalfluxlist = NULL;
// used when reading grids these are set then calls
// to read the grid are made later, file format causes this
bool gridtype = false;
bool polartype = false;
// specifies what type grids should be
// converted to in order to work with.
char workingtype[4096];

#define PI 3.1415926535897932384626433832795

/*******************************************************************************
* adds a flux to the global flux list.  the fluxname
* variable of flux is modified if it conflicts with
* an existing flux that it cannot be merged
* with.  if has found a matching flux then the
* fluxname will also be changed to the name
* of the matching flux.  if flux does not match
* nor conflict with any existing global fluxes
* then the fluxname is not modified.
* in all cases, though, flux->oldfluxname will
* save a copy of the fluxname before it is
* modified to the new flux name.  this makes
* it easy to associate a flux local to a dataset
* with a global flux (you can think of fluxname
* as being globalfluxname, whereas
* oldfluxname is localfluxname).

  * returns true if the flux is pointed at by globalfluxlist
  * if addFlux returns false it does not mean
  * it failed, it just means that your flux is not
  * referenced by globalfluxlist.
*/

void clearfluxlist()
{
  if (!globalfluxlist) delete[] globalfluxlist;
  globalfluxlist = NULL;
  globalfluxnum = 0;
}

bool addFlux(ATOFluxType *flux)
{
  int i;
  char buff[SMALLSTRING];

  // copy name over to old name for saving
  rstrcpy(flux->oldfluxname, flux->fluxname);
  if (!globalfluxlist)
  {
    flux->index = 1;
    rstrcpy(flux->fluxname, flux->type);
    strcat(flux->fluxname, " 1");
    globalfluxlist = new ATOFluxType*[1];
    globalfluxlist[0] = flux;
    globalfluxnum++;
    return true;
  }

  // search for exact flux match
  for (i = 0; i < globalfluxnum; i++)
    if (!rstrcmpi(globalfluxlist[i]->type, flux->type) &&
      isEqual(globalfluxlist[i]->density, flux->density) &&
      isEqual(globalfluxlist[i]->radius, flux->radius))  break;

  if (i < globalfluxnum)
  {
    // set index to new global
    flux->index = globalfluxlist[i]->index;
    // a flux match was found if i points to an item in the list
    rstrcpy(flux->fluxname, globalfluxlist[i]->fluxname);
    return false;
  }

  // add flux
  ATOFluxType **temp = globalfluxlist;
  globalfluxlist = new ATOFluxType*[globalfluxnum + 1];

  // set index to start counting
  flux->index = 1;
  // copy over the existing fluxes
  for (i = 0; i < globalfluxnum; i ++)
  {
    globalfluxlist[i] = temp[i];
    // set the index count of the flux for this type
    if (!rstrcmpi(globalfluxlist[i]->type, flux->type))
      flux->index++;
  }

  // add new flux
  itoa(flux->index, buff, 10);
  rstrcpy(flux->fluxname, flux->type, " ", buff);
  globalfluxlist[i] = flux;
  globalfluxnum++;

  // delete old flux array
  delete [] temp;
  return true;
}

/*
* take x, y cartesian points and convert to
* radius and angle (polar).
x will contain the radius
* y will contain the angle in degrees
* returns false if something went wrong
* assumes x, y relative to center of 0, 0
*/
bool convertToCompass(double &x, double &y)
{

  if (x == 0.0 && y == 0.0)   return true;
  double angle = atan(y / x);
  double radius = sqrt(pow(x, 2.0) + pow(y, 2.0));
  angle = angle * 180 / PI;
/*
     correct angle for quadrant
      -x,y    \/   x,y
      -x,-y   /\   x,-y
*/
  if (x>0 && y>=0)     angle=90.0-angle;    
  if (x>=0 && y<0)     angle=90.0-angle;     
  if (x<0 && y<=0)     angle=270.0-angle; 
  if (x<=0 && y>0) 
  { 
    angle=270.0-angle; 
    if (angle==180.0)
      angle = 360.0;
  }
 
  x = radius;
  y = angle;

  return true;
}

/*
* take radius and angle and convert to x, y
* cartesian points.  radius will contain the
* x coordinate and angle will contain the
* y coordinate when the function completes.
* returns false if something went wrong
*/
bool convertFromCompass(double &radius, double &angle)
{
  if (radius == 0.0 && angle == 0.0)  
    return true;
  angle = 450.0 - angle;
  if (angle >= 360 )
    angle = angle-360;
  double radians = angle * PI / 180.0;
  // x = r cos (theta) = radius * cos(angle)
  double x = radius * cos(radians);
  // y = r sin (theta) = radius * sin(angle)
  double y = radius * sin(radians);
  if (fabs(x)<1.0e-6) x = 0;
  if (fabs(y)<1.0e-6) y = 0;
  radius = x;
  angle = y;
  return true;
}

/*
* take x, y cartesian points and convert to
* radius and angle (polar).
x will contain the radius
* y will contain the angle in degrees
* returns false if something went wrong
* assumes x, y relative to center of 0, 0
*/
bool convertToPolar(double &x, double &y)
{
  if (x == 0.0 && y == 0.0)   return true;
  double angle = atan(y / x);
  double radius = sqrt(pow(x, 2.0) + pow(y, 2.0));
  angle = angle * 180 / PI;

  // correct angle for quadrant
  if (x>0 && y>0) {}
  if (x>0 && y<0) { angle+=360.0; }
  if (x<0 && y<0) { angle+=180.0; }
  if (x<0 && y>0) { angle+=180.0; }

  x = radius;
  y = angle;

  return true;
}

/*
* take radius and angle and convert to x, y
* cartesian points.  radius will contain the
* x coordinate and angle will contain the
* y coordinate when the function completes.
* returns false if something went wrong
*/
bool convertFromPolar(double &radius, double &angle)
{

  if (radius == 0.0 && angle == 0.0)  return true;
  double radians = angle * PI / 180.0;
  // x = r cos (theta) = radius * cos(angle)
  double x = radius * cos(radians);
  // y = r sin (theta) = radius * sin(angle)
  double y = radius * sin(radians);

  radius = x;
  angle = y;
  return true;
}

/*
* merge to lists (assumed sorted).  items that
* ie,
* 1 3 5 6 7
*     merged with
* 1 2 3 4 5 8 9
*     becomes
* 1 2 3 4 5 6 7 8 9
*
* the result size of the list will be stored in num.
* the result values will be stored in values.  it is
* the callers responsibility to allocate enough
* spaces in values to store the merge (that
* means at max, values will need to be
* allocated to list1num + list2num).
*/
void merge(int list1num, double *list1values, int list2num, double *list2values, int &num, double *values)
{
  num = 0;
  int pos1 = 0, pos2 = 0;
  while(pos1 < list1num && pos2 < list2num)
  {
    if (list1values[pos1] < list2values[pos2])
    {
      values[num] = list1values[pos1];
      pos1++;
    }
    else if (list1values[pos1] > list2values[pos2])
    {
      values[num] = list2values[pos2];
      pos2++;
    }
    else
    {
      values[num] = list1values[pos1];
      pos1++;
      pos2++;
    }
    num++;
  }
  while(pos1 < list1num)
  {
    values[num] = list1values[pos1];
    num++;
    pos1++;
  }
  while(pos2 < list2num)
  {
    values[num] = list2values[pos2];
    num++;
    pos2++;
  }
}

// -----------------------------------------------------------------------------
// class ATOGrid
ATOGrid::ATOGrid()
{
  axis1num = 0;
  axis2num = 0;
  rstrcpy(dataunits, "");
  rstrcpy(axis1units, "");
  rstrcpy(axis2units, "");
  rstrcpy(moist, "");
  rstrcpy(type, "");
  axis2values = NULL;
  axis1values = NULL;
  values = NULL;
  flux = NULL;
  linear = false;
}

ATOGrid::ATOGrid(ATOGrid* grid)
{
  int i, j;

  linear = grid->linear;
  if (!linear)
  {
    axis1num = grid->axis1num;
    axis2num = grid->axis2num;
    rstrcpy(dataunits, grid->dataunits);
    rstrcpy(axis1units, grid->axis1units);
    rstrcpy(axis2units, grid->axis2units);
    rstrcpy(moist, grid->moist);
    // rstrcpy(fluxtype, grid->fluxtype);
    flux = grid->flux;
    rstrcpy(type, grid->type);
    axis1values = new double[axis1num];
    for (i = 0; i<axis1num; i++)
      axis1values[i] = grid->axis1values[i];
    axis2values = new double[axis2num];
    for (i = 0; i<axis2num; i++)
      axis2values[i] = grid->axis2values[i];
    values = new double*[axis1num];
    for (i = 0; i<axis1num; i++)
      values[i] = new double[axis2num];
    for (i = 0; i<axis1num; i++)
      for (j = 0; j<axis2num; j++)
        values[i][j] = grid->values[i][j];
  }
  else
  {
    axis1num = 0;
    axis2num = 0;
    axis2values = NULL;
    axis1values = NULL;
    values = NULL;
  }
}

ATOGrid::~ATOGrid()
{
  if (axis1values) delete[] axis1values;
  if (axis2values) delete[] axis2values;
  if (values)
  {
    for (int i = 0; i<axis1num; i++)
      if (values[i])
        delete[] values[i];
    delete[] values;
  }
  rstrcpy(dataunits, "");
  rstrcpy(axis1units, "");
  rstrcpy(axis2units, "");
  rstrcpy(moist, "");
  rstrcpy(type, "");
  axis1num = 0;
  axis2num = 0;
  axis2values = NULL;
  axis1values = NULL;
  values = NULL;
  flux = NULL;
  linear = false;
}

double ATOGrid::GetGridMax(double& xcoord, double& ycoord)
{
  int i, j;
  double tempmax;

  tempmax = values[0][0];
  xcoord = axis1values[0];
  ycoord = axis2values[0];
  int ysize = linear ? 1 : axis2num;
  for (i = 0; i<axis1num; i++)
    for (j = 0; j<ysize; j++)
      if (values[i][j] > tempmax)
      {
        tempmax = values[i][j];
        xcoord = (double)axis1values[j];
        ycoord = (double)axis2values[i];
      }
  return (double)tempmax;
}

double ATOGrid::GetGridMin(double& xcoord, double& ycoord)
{
  int i, j;
  double tempmin;

  tempmin = 99999.00;
  xcoord = axis1values[0];
  ycoord = axis2values[0];
  int ysize = linear ? 1 : axis2num;
  for (i = 0; i<axis1num; i++)
    for (j = 0; j<ysize; j++)
      if (values[i][j] < tempmin && values[i][j] != 0.0)
      {
        tempmin = values[i][j];
        xcoord = (double)axis1values[j];
        ycoord = (double)axis2values[i];
      }
  return (double)tempmin;
}

ATOGrid* ATOGrid::ZoomGridX2(void)
{
  if (linear)
    return 0;
  int xcount, ycount;
  int i, j, x, y;
  xcount = 0;
  ycount = 0;
  ATOGrid *tempgrid = new ATOGrid();
  tempgrid->axis1num = axis1num + (axis1num - 1);
  tempgrid->axis2num = axis2num + (axis2num - 1);
  tempgrid->axis1values = new double[tempgrid->axis1num];
  tempgrid->axis2values = new double[tempgrid->axis2num];
  for (i = 0; i<tempgrid->axis1num; i++)
    if (i%2 == 0)
    {
      tempgrid->axis1values[i] = axis1values[xcount];
      xcount++;
    }
  for (i = 0; i<tempgrid->axis1num; i++)
    if (i%2 != 0)
      tempgrid->axis1values[i] = (tempgrid->axis1values[i-1]+tempgrid->axis1values[i+1])/2;
  for (i = 0; i<tempgrid->axis2num; i++)
    if (i%2 == 0)
    {
      tempgrid->axis2values[i] = axis2values[ycount];
      ycount++;
    }
  for (i = 0; i<tempgrid->axis2num; i++)
    if (i%2 != 0)
      tempgrid->axis2values[i] = (tempgrid->axis2values[i-1]+tempgrid->axis2values[i+1])/2;
  tempgrid->values = new double*[tempgrid->axis2num];
  for (j = 0; j < tempgrid->axis2num; j++)
    tempgrid->values[j] = new double[tempgrid->axis1num];
  // xcount = 0;
  // ycount = 0;
  // interpolate the grid
  int ysize = linear ? 1 : axis2num;
  for (i = 0; i<ysize; i++)
  {
    xcount = 0;
    for (j = 0; j<axis1num; j++)
    {
      tempgrid->values[xcount][ycount] = values[j][i];
      xcount+= 2;
    }
    ycount+= 2;
  }

  int tempgrid_ysize = tempgrid->linear ? 1 : tempgrid->axis2num;
  for (y = 0; y<tempgrid_ysize; y+= 2)
    for (x = 0; x<tempgrid->axis1num; x++)
      if (x%2 != 0)
        tempgrid->values[x][y] = (tempgrid->values[x-1][y] + tempgrid->values[x+1][y])/2;

  for (x = 0; x<tempgrid->axis1num; x++)
    for (y = 0; y<tempgrid_ysize; y++)
      if (y%2 != 0)
        tempgrid->values[x][y] = (tempgrid->values[x][y-1] + tempgrid->values[x][y+1])/2;

  return tempgrid;
}

bool ATOGrid::matches(ATOGrid* other)
{
  if (!other ||
    rstrcmpi(dataunits, other->dataunits) != 0 ||
    rstrcmpi(axis1units, other->axis1units) != 0 ||
    rstrcmpi(moist, other->moist) != 0 ||
    rstrcmpi(flux->fluxname, other->flux->fluxname) != 0 ||
    rstrcmpi(type, other->type))
    return false;
  else
    return true;
}

bool ATOGrid::synchGrids(ATOGrid* other)
{
  bool diffTypes = rstrcmpi(axis2units, other->axis2units) != 0;
  if (!other || !matches(other) || linear)
    return false;

  //  line up grids if necessary
  if (!compareAxis(other) || diffTypes)
  {
    char polar[] = "polar";
    char cartesian[] = "cartesian";
    char *type = 0;
    if (diffTypes && isPolar())
      type = polar;
    else if (diffTypes)
      type = cartesian;

    //  merge grid axis values (this assumes axis values are ordered)
    int newaxis1num = 0, newaxis2num = 0;
    int maxaxis1num = axis1num + other->axis1num;
    int maxaxis2num = axis2num + other->axis2num;
    double *newaxis1values = new double[maxaxis1num];
    double *newaxis2values = new double[maxaxis2num];

    merge(axis1num, axis1values, other->axis1num, other->axis1values,
      newaxis1num, newaxis1values);
    merge(axis2num, axis2values, other->axis2num, other->axis2values,
      newaxis2num, newaxis2values);

    // copy and convert the grid
    convert(newaxis1num, newaxis1values, newaxis2num, newaxis2values, type);
    other->convert(newaxis1num, newaxis1values, newaxis2num, newaxis2values, type);
    delete [] newaxis1values;
    delete [] newaxis2values;
  }
  return true;
}

bool ATOGrid::compareAxis(ATOGrid *other)
{
  if (!other || (axis1num != other->axis1num && axis2num != other->axis2num))
    return false;

  for (int i = 0; i < axis1num; i++)
    if (axis1values[i] != other->axis1values[i])
      return false;

  for (int j = 0; j < axis2num; j++)
    if (axis2values[j] != other->axis2values[j])
      return false;

  return true;
}

bool ATOGrid::add(ATOGrid *other)
{
  if (!synchGrids(other))
    return false;

  // grids should be lined up by now, add values
  for (int j = 0; j < axis2num; j++)
    for (int i = 0; i < axis1num; i++)
      values[i][j] += other->values[i][j];

  return true;
}

void ATOGrid::getBounds(double &minX, double &minY, double &maxX, double &maxY, const char *type)
{
  double x, y;
  int i, j;

  bool polar = isPolar();
  bool toPolar = false;
  // this bool gives code clarity
  bool toCartesian = false;
  if (type)
  {
    toPolar = !rstrcmpi(type, POLAR) && !polar;
    toCartesian = !toPolar && polar;
  }

  minX = 99999999.0;
  if (polar || toPolar)
    minY = 360.0;
  else
    minY = 99999999.0;
  maxX = 0.0;
  maxY = 0.0;

  int ysize = linear ? 1 : axis2num;
  for (j = 0; j < ysize; j++)
  {
    for (i = 0; i < axis1num; i++)
    {
      x = axis1values[i];
      if (linear)
        y = axis2values[i];
      else
        y = axis2values[j];

      // convert if according to the type argument
      if (toPolar)
        convertToCompass(x, y);
      else if (toCartesian)
        convertFromCompass(x, y);

      // min and max X
      if (x < minX)
        minX = x;
      if (x > maxX)
        maxX = x;

      if ((polar && !toCartesian) || toPolar)
      {
        // min and max Y
        if (y < minY && y >= 0.0)
          minY = y;
        if (y > maxY && y <= 360.0)
          maxY = y;
      }
      else
      {
        // min and max Y
        if (y < minY)
          minY = y;
        if (y > maxY)
          maxY = y;
      }
    }
  }
}

bool ATOGrid::isPolar()
{
  if (!rstrcmpi(axis2units, "deg"))
    return true;
  else
    return false;
}

void ATOGrid::convert(int sizeX, double *listX, int sizeY, double *listY, const char *type)
{

  int i, j;
  if (linear || (sizeX < 0 || sizeY < 0) || (type == 0 && sizeX == 0 && sizeY  == 0) ||
    (sizeX <= 0 && sizeY <= 0 && listX  == 0 && listY == 0 && ((!rstrcmpi(type, POLAR) && isPolar()) ||
    (!rstrcmpi(type, CARTESIAN) && !isPolar()))))
    return;

  bool sameType = !type;
  bool toPolar = false;
  // this bool gives code clarity
  bool toCartesian = false;
  bool polar = isPolar();
  if (type)
  {
    toPolar = !rstrcmpi(type, POLAR) && !polar;
    toCartesian = !toPolar && polar;
    sameType = (!toCartesian && !toPolar);
  }

  double minX = 0.0, minY = 0.0, maxX = 0.0, maxY = 0.0;
  int newaxis1num = sizeX;
  int newaxis2num = sizeY;

  double *newaxis1values;
  double *newaxis2values;

  // get bounds in the units of the type we are converting to
  getBounds(minX, minY, maxX, maxY, type);

  // construct the axis1 values
  if (!sameType && sizeX == 0 || (sizeX > 0 && listX == 0))
  {
    // if sizeX is specified, but listX is not, the user just wants to
    // resize the xaxis.
    if (sizeX > 0 && listX == 0)
      newaxis1num = sizeX;
    else
      // otherwise sizeX is 0, but needs to be converted
      newaxis1num = axis1num;
    newaxis1values = new double[newaxis1num];
    double stepX = (maxX - minX) / (double)(newaxis1num-1);

    // fill x axis values
    newaxis1values[0] = minX;
    for (i = 1; i < newaxis1num; i++)
      newaxis1values[i] = newaxis1values[i-1] + stepX;
  }
  else if (sizeX  == 0)
  {
    // user wants to avoid changes to x axis
    newaxis1num = axis1num;
    newaxis1values = axis1values;
  }
  else
  {
    // user supplied new list for x axis values
    newaxis1values = new double[newaxis1num];
    for (i = 0; i < newaxis1num; i++)
      newaxis1values[i] = listX[i];
  }

  // construct the axis2 values
  if (!sameType && sizeY == 0 || (sizeY > 0 && listY == 0))
  {
    // if sizeY is specified, but listY is not, the user just wants to
    // resize the yaxis.
    if (sizeY > 0 && listY == 0)
      newaxis2num = sizeY;
    else
      // otherwise sizeY is 0, but needs to be converted
      newaxis2num = axis2num;
    newaxis2values = new double[newaxis2num];
    double stepY = (maxY - minY) / (double)(newaxis2num-1);

    // fill x axis values
    newaxis2values[0] = minY;
    for (j = 1; j < newaxis2num; j++)
      newaxis2values[j] = newaxis2values[j-1] + stepY;
  }
  else if (sizeY  == 0)
  {
    // user wants to avoid changes to y axis
    newaxis2num = axis2num;
    newaxis2values = axis2values;
  }
  else
  {
    // user supplied new list for y axis values
    newaxis2values = new double[newaxis2num];
    for (j = 0; j < newaxis2num; j++)
      newaxis2values[j] = listY[j];
  }

  // create value grid
  double **newvalues = new double*[newaxis1num];
  for (i = 0; i < newaxis1num; i++)
    newvalues[i] = new double[newaxis2num];

  // closest three points
  int xindexes[3], yindexes[3];
  double x[3], y[3];
  int v;
  double oldValues[3];
  double value, targetX = 0.0, targetY = 0.0;
  for (j = 0; j < newaxis2num; j++)
  {
    for (i = 0; i < newaxis1num; i++)
    {
      // get the closest three points and store in lists x and y
      //  can pass indices for both x an y lists because they
      //  should be the same for a linear list
      targetX = newaxis1values[i];
      targetY = newaxis2values[j];
      // newaxis values will be polar if the destination grid is in polar units
      // or it was already in polar and is not being converted to cartesian
      if (toPolar || (polar && !toCartesian))
        convertFromCompass(targetX, targetY);
      closestThree(targetX, targetY, xindexes, yindexes);
      for (v = 0; v < 3; v++)
      {
        oldValues[v] = values[xindexes[v]][yindexes[v]];
        // get values from the three point indexes
        x[v] = axis1values[xindexes[v]];
        y[v] = axis2values[yindexes[v]];
        if (polar)
          convertFromCompass(x[v], y[v]);
      }
      value = interpolate(targetX, targetY, x, y, oldValues);

      newvalues[i][j] = value;
    }
  }
  // delete old values
  if (!sameType || sizeX != 0)
    delete []axis1values;
  if (!sameType || sizeY != 0)
    delete []axis2values;
  for (i = 0; i<axis1num; i++)
    delete[] values[i];
  delete[] values;

  axis1values = newaxis1values;
  axis2values = newaxis2values;
  values = newvalues;
  axis1num = newaxis1num;
  axis2num = newaxis2num;

  if (!sameType)
  {
    if (!rstrcmpi(type, POLAR))
      rstrcpy(axis2units, "deg");
    else
      rstrcpy(axis2units, "m");
  }
}

void ATOGrid::linearToGrid()
{
  if (!linear)
    return;

  bool polar = isPolar();
  int i, j, length = axis1num;
  double minX = 0.0, minY = 0.0, maxX = 0.0, maxY = 0.0;

  getBounds(minX, minY, maxX, maxY);
  // create square grid
  int newaxis1num = (int)ceil(sqrt((double)length));
  int newaxis2num = newaxis1num;

  double *newaxis1values = new double[newaxis1num];
  double *newaxis2values = new double[newaxis2num];

  double stepX = 0.0;
  double stepY = 0.0;
  if (newaxis1num>1)
    stepX = (maxX - minX) / (double)(newaxis1num-1);
  if (newaxis2num>1)
    stepY = (maxY - minY) / (double)(newaxis2num-1);

  // fill x axis values
  newaxis1values[0] = minX;
  for (i = 1; i < newaxis1num; i++)
    newaxis1values[i] = newaxis1values[i-1] + stepX;

  // fill y axis values
  newaxis2values[0] = minY;
  for (j = 1; j < newaxis2num; j++)
    newaxis2values[j] = newaxis2values[j-1] + stepY;

  // create value grid
  double **newvalues = new double*[newaxis1num];
  for (i = 0; i < newaxis1num; i++)
    newvalues[i] = new double[newaxis2num];

  // closest three points
  int indexes[3];
  double x[3], y[3];
  int v;
  double oldValues[3];
  double value, targetX = 0.0, targetY = 0.0;
  for (j = 0; j < newaxis2num; j++)
  {
    for (i = 0; i < newaxis1num; i++)
    {
      targetX = newaxis1values[i];
      targetY = newaxis2values[j];
      if (polar)
        convertFromCompass(targetX, targetY);
      // get the closest three points and store in lists x and y
      //  can pass indices for both x an y lists because they
      //  should be the same for a linear list
      closestThree(targetX, targetY, indexes, indexes);
      for (v = 0; v < 3; v++)
      {
        oldValues[v] = values[indexes[v]][0];
        // get values from the three point indexes
        x[v] = axis1values[indexes[v]];
        y[v] = axis2values[indexes[v]];
        if (polar)
          convertFromCompass(x[v], y[v]);
      }
      value = interpolate(targetX, targetY, x, y, oldValues);

      newvalues[i][j] = value;
    }
  }
  // delete old values
  delete[] axis1values;
  delete[] axis2values;
  for (i = 0; i<axis1num; i++)
    delete[] values[i];
  delete[] values;

  axis1values = newaxis1values;
  axis2values = newaxis2values;
  values = newvalues;
  axis1num = newaxis1num;
  axis2num = newaxis2num;

  // cout << "(" << minX << ", " << minY << ")"
  // << " - (" << maxX << ", " << maxY << ")" << endl;
}
// ---------------------------------------------------------------------------
bool ATOGrid::closestThree(double x, double y, int* listX, int* listY)
{
  if (!values)
    return false;

  bool polar = isPolar();
  double distance;
  double distances[] = {9999999999.7, 9999999999.8, 9999999999.9};
  double currX, currY;
  int i, j, t, z;

  // if (polar)
  // convertFromCompass(x, y);

  int ysize = linear ? 1 : axis2num;
  for (j = 0; j < ysize; j++)
  {
    for (i = 0; i < axis1num; i++)
    {
      if (linear) j = i;

      currY = axis2values[j];
      currX = axis1values[i];
      if (polar)
        convertFromCompass(currX, currY);

      // now currX and currY are in cartesian, find difference from x, y
      distance = sqrt(pow(currX-x, 2.0) + pow(currY-y, 2.0));
      // return if there is an exact distance
      if (distance == 0.0)
      {
        for (t = 0; t < 3; t++)
        {
          distances[t] = distance;
          listX[t] = i;
          listY[t] = j;
        }
        return true;
      }

      // insert distance and point if it's small enough
      // shift the rest of the distances and points down
      for (int t = 0; t < 3; t++)
      {
        if (distance < distances[t])
        {
          for (z = 2; z > t; z--)
          {
            distances[z] = distances[z-1];
            listX[z] = listX[z-1];
            listY[z] = listY[z-1];
          }
          distances[t] = distance;
          listX[t] = i;
          listY[t] = j;
          break;
        }
      }
    }
  }
  return false;
}

void ATOGrid::offset(int x, int y)
{
  if (x == 0 && y == 0)
    return;
    /*
    // can't offset a polar grid and keep the axis forms at the same time
    convert(0, 0, 0, 0, "cartesian");
  */

  for (int i = 0; i < axis1num; i++)
    axis1values[i] += x;

  for (int j = 0; j < axis2num; j++)
    axis2values[j] += y;

}
// ---------------------------------------------------------------------------
bool ATOGrid::Read(icsv* infile)
{
  int i, j;
  double val;
  char dummy[256];
  char fluxname[SMALLSTRING];
  *infile >> type >> fluxname >> moist >> dataunits >> axis1num >> axis1units >> axis2num >> axis2units >> NewLn;
  // if reading, then currentDataset must also be reading
  // (which is what dataset this grid belongs to)
  // the flux types for currentDataset have already been
  // merged with flux types from other datasets.  it's
  // possible that the flux name was changed in the
  // merge process.  however, currentDataset saves the
  // old flux names with the flux types, and provides
  // getFlux for the purpose of grabbing the new flux
  // associated with the old flux name.
  flux = currentDataset->getFlux(fluxname);
  // if (flux)
  // rstrcpy(fluxtype, flux->fluxname);

  if (!gridtype)
  {
    for (i = 0; i<axis1num; i++)
      *infile >> dummy;
    *infile >> NewLn;
  }
  axis1values = new double[axis1num];
  for (i = 0; i<axis1num; i++)
    *infile >> axis1values[i];
  *infile >> NewLn;
  values = new double*[axis1num];
  for (i = 0; i < axis1num; i++)
    values[i] = new double[axis2num];

  if (gridtype)
  {
    axis2values = new double[axis2num];

    for (j = 0; j < axis2num; j++)
    {
      *infile >> axis2values[j];
      for (i = 0; i < axis1num; i++)
      {
        *infile >> val;
        values[i][j] = val;
      }
      *infile >> NewLn;
    }
  }
  else
  {
    axis2num = axis1num;
    axis2values = new double[axis2num];

    for (i = 0; i<axis2num; i++)
      *infile >> axis2values[i];

    *infile >> NewLn;
    *infile >> val; // skip -99
    for (i = 0; i<axis1num; i++)
    {
      *infile >> val;
      values[i][0] = val;
    }
    *infile >> NewLn;

    linear = true;
    // convert the linear input to a uniform grid
    linearToGrid();
    linear = false;
  }

  if (!rstrcmpi(workingtype, "cartesian") || !rstrcmpi(workingtype, "polar"))
    convert(0, 0, 0, 0, workingtype);

  // offset the grid to the global x, y specified by ATO.Read()
  offset(g_x, g_y);
  return true;
}
// ---------------------------------------------------------------------------
void ATOGrid::Write(ocsv* outfile, bool writelinear)
{
  int i, j;
  // convert(4, 0, 4, 0);
  *outfile << type << flux->getFullName() << moist << dataunits;
  // output (null) descriptors for points
  if (writelinear)
  {
    int total;
    total = axis1num;

    *outfile << total << axis1units << 1 << axis2units << NewLn;
    for (i = 0; i < total; i++)
      *outfile << "";
    *outfile << NewLn;

    for (i = 0; i<total; i++)
      *outfile << axis1values[i%axis1num];
    *outfile << NewLn;

    for (j = 0; j<total; j++)
      *outfile << axis2values[j%axis2num];
    *outfile << NewLn;

    for (j = 0; j<1; j++)
      for (i = 0; i<axis1num; i++)
        *outfile << values[i][j];
      *outfile << NewLn;
  }
  else
  {
    *outfile << axis1num << axis1units << axis2num << axis2units << NewLn;
    for (i = 0; i<axis1num; i++)
      *outfile << axis1values[i];
    *outfile << NewLn;

    for (j = 0; j<axis2num; j++)
    {
      *outfile << axis2values[j];
      for (i = 0; i<axis1num; i++)
        *outfile << values[i][j];
      *outfile << NewLn;
    }
  }
}

double ATOGrid::getValue(double xcoord, double ycoord)
{
  int i;
  int xPosition = -1;
  int yPosition = -1;
  for (i = 0; i<axis1num; i++){
    if (axis1values[i] == xcoord){
      xPosition = i;
      break;
    }
  }
  for (i = 0; i<axis2num; i++){
    if (axis2values[i] == ycoord){
      yPosition = i;
      break;
    }
  }
  if (linear) yPosition = 0;
  if (xPosition < 0 || yPosition < 0)
    return 0.0f;
  else
    return values[xPosition][yPosition];
}

int ATOGrid::getAxis1index(double xcoord)
{
  for (int i = 0; i<axis1num; i++)
    if (axis1values[i] == xcoord)
      return i;
  return -1;
}

int ATOGrid::getAxis2index(double ycoord)
{
  for (int i = 0; i<axis2num; i++)
    if (axis2values[i] == ycoord)
      return i;
  return -1;
}

// ------------------------------------------------------------------------------
// class ATOTimePeriod
ATOTimePeriod::ATOTimePeriod()
{
  time = 0;
  numoutput = 0;
  rstrcpy(unit, "");
  grids = new ATOGrid*[STEP_SIZE];
}

ATOTimePeriod::~ATOTimePeriod()
{
  int i;
  for (i = 0; i<numoutput; i++)
    delete grids[i];
  delete grids;
  time = 0;
  numoutput = 0;
  rstrcpy(unit, "");
}

bool ATOTimePeriod::Read(icsv* infile)
{
  int i, numout;
  *infile >> time >> unit >> numout >> NewLn;
  // grids = new ATOGrid*[numoutput];
  for (i = 0; i<numout; i++)
    appendGrid(new ATOGrid());
  for (i = 0; i<numoutput; i++)
    grids[i]->Read(infile);
  return true;
}

void ATOTimePeriod::Write(ocsv* outfile)
{
  int i;
  *outfile << time << unit << numoutput << NewLn;
  for (i = 0; i<numoutput; i++)
    grids[i]->Write(outfile, writelinear);
}

double ATOTimePeriod::getValue(int gridIdx, double axis1value, double axis2value)
{
  return grids[gridIdx]->getValue(axis1value, axis2value);
}

ATOGrid* ATOTimePeriod::getGrid(char *product, char *flux, char *moist)
{
  for (int j = 0; j<numoutput; j++){
    if (!rstrcmpi(trim(grids[j]->type), trim(product)) &&
       (!rstrcmpi(trim(grids[j]->flux->fluxname), trim(flux)) ||
        !rstrcmpi(trim(grids[j]->flux->oldfluxname), trim(flux))) &&
        !rstrcmpi(trim(grids[j]->moist), trim(moist)))
      return grids[j];
  }
  return NULL;
}

ATOGrid* ATOTimePeriod::getGrid(int gridIdx)
{
  if (gridIdx>-1 && gridIdx<numoutput)
    return grids[gridIdx];
  return NULL;
}

void ATOTimePeriod::appendGrid(ATOGrid *grid)
{
  int i;
  // only resize the array every "size" elements to make things faster
  int upperlimit = ((numoutput / STEP_SIZE) + 1) * STEP_SIZE;
  // only need to resize if numoutput is just below the upperlimit
  if (upperlimit - 1 == numoutput)
  {
    // save the current grids to a temp spot while we resize the array
    ATOGrid **temp = new ATOGrid*[numoutput];
    for (i = 0; i < numoutput; i++)
    {
      temp[i] = grids[i];
      grids[i] = 0;
    }
    delete grids;
    grids = new ATOGrid*[upperlimit+STEP_SIZE];
    for (i = 0; i < numoutput; i++)
    {
      grids[i] = temp[i];
      temp[i] = 0;
    }
    delete temp;
  }
  grids[numoutput] = grid;
  numoutput++;
}

bool ATOTimePeriod::add(ATOTimePeriod *other)
{
  if (time != other->time || rstrcmpi(unit, other->unit) != 0)
    return false;

  for (int j = 0; j < other->numoutput; j++)
  {
    ATOGrid *grid = getGrid(other->grids[j]->type, other->grids[j]->flux->fluxname, other->grids[j]->moist);
    if (grid)
      grid->add(other->grids[j]);
    else
      appendGrid(new ATOGrid(other->grids[j]));
  }

  return true;
}

ATOTimePeriod *ATOTimePeriod::copy()
{
  ATOTimePeriod *cop = new ATOTimePeriod();
  cop->time = time;
  rstrcpy(cop->unit, unit);
  // cop->numoutput = numoutput;
  for (int i = 0; i < numoutput; i++)
    cop->appendGrid(new ATOGrid(grids[i]));

  return cop;
}

// ------------------------------------------------------------------------------
// class ATOCon
ATOCon::ATOCon(bool progeny)
{
  numtimes = 0;
  numprogs = 0;
  validCount = 0;
  progs = NULL;
  times = NULL;
  validTimes = NULL;
  rstrcpy(product, "");
  rstrcpy(flux, "");
  rstrcpy(moist, "");
  rstrcpy(units, "");
  rstrcpy(chemname, "");
  rstrcpy(casid, "");
  rstrcpy(parenname, "");
  rstrcpy(parencasid, "");
  isprogeny = progeny;
  xcoord = -1;
  ycoord = -1;
}

ATOCon::~ATOCon()
{
  int i;

  if (!isProgeny())
  {
    if (progs !=  NULL)
    {
      for (i = 0; i<numprogs; i++)
        delete progs[i];
      delete [] progs;
    }
  }
  if (times)
  {
    for (i = 0; i<numtimes; i++)
      delete times[i];
    delete times;
  }
  numtimes = 0;
  numprogs = 0;
  validCount = 0;
  progs = NULL;
  times = NULL;
  validTimes = NULL;
  rstrcpy(product, "");
  rstrcpy(flux, "");
  rstrcpy(moist, "");
  rstrcpy(units, "");
  rstrcpy(chemname, "");
  rstrcpy(casid, "");
  rstrcpy(parenname, "");
  rstrcpy(parencasid, "");
  isprogeny = false;
  xcoord = -1;
  ycoord = -1;
}

bool ATOCon::Read(icsv* infile)
{
  int i;
  *infile >> chemname >> casid >> numtimes;
  if (!isProgeny())
    *infile >> numprogs;
  else
  {
    *infile >> parenname >> parencasid;
    numprogs = 0;
  }
  *infile >> NewLn;

  times = new ATOTimePeriod*[numtimes];
  for (i = 0; i<numtimes; i++)
    times[i] = new ATOTimePeriod();
  for (i = 0; i<numtimes; i++)
    times[i]->Read(infile);
  if (!isProgeny())
  {
    if (numprogs > 0)
    {
      progs = new ATOCon*[numprogs];
      for (i = 0; i<numprogs; i++)
        // pass true to ATOCon specifying that it's a progeny
        progs[i] = new ATOCon(true);
      for (i = 0; i<numprogs; i++)
        progs[i]->Read(infile);
    }
  }
  else
    progs = 0;
  return true;
}

void ATOCon::Write(ocsv* outfile, bool writeprog)
{
  int i;
  *outfile << chemname << casid << numtimes;
  if (isProgeny())
    if (writeprog)
      *outfile << parenname << parencasid;
    else
      *outfile << numprogs;
  else
    *outfile << numprogs;
  *outfile << NewLn;

  for (i = 0; i<numtimes; i++)
    times[i]->Write(outfile);
  if (!isProgeny())
    if (writeprog)
      for (i = 0; i<numprogs; i++)
        progs[i]->Write(outfile);
}

ATOCon* ATOCon::add(ATOCon *other)
{
  if (!other ||
     rstrcmpi(chemname, other->chemname) != 0 ||
     rstrcmpi(casid, other->casid) != 0 ||
    (isProgeny() &&
    (rstrcmpi(parenname, other->parenname) != 0 ||
     rstrcmpi(parencasid, other->parencasid) != 0 ))
    )
    return 0;

  // store results in this contamination object. init it.
  ATOCon *resultContam = new ATOCon(isProgeny());
  rstrcpy(resultContam->chemname, chemname);
  rstrcpy(resultContam->casid, casid);
  resultContam->numtimes = 0;
  resultContam->numprogs = 0;
  resultContam->times = new ATOTimePeriod*[numtimes + other->numtimes];
  if (!isProgeny() && (numprogs + other->numprogs) > 0)
    resultContam->progs = new ATOCon*[numprogs + other->numprogs];
  if (isProgeny())
  {
    rstrcpy(resultContam->parenname, parenname);
    rstrcpy(resultContam->parencasid, parencasid);
  }

  // holds the interpolated time period
  ATOTimePeriod *interpolated;
  int thisPos = 0, otherPos = 0;

  // not sure exactly why we want to verify the ends to have
  // zero values
  // ensureEndZeros();
  // other->ensureEndZeros();

  // while there are records left to interpolate
  while(thisPos < numtimes && otherPos < other->numtimes)
  {
    // thisPos value should be inserted before otherPos
    // insert the time in the result recordset, and estimate
    // the value that should be added to it for interpolation
    // (it should be between otherPos-1 and otherPos)
    if (times[thisPos]->time < other->times[otherPos]->time)
    {
      // otherPos > 0 and record.time should be in
      // between time at [otherPos-1] and time at [otherPos], so
      // use records at otherPos-1 and otherPos to estimate the
      // value at times[thisPos] so it can be added
      if (otherPos > 0)
      {
        interpolated = interpolateTimePeriod(other->times[otherPos-1],
          other->times[otherPos],
          times[thisPos]->time);
        interpolated->add(times[thisPos]);
      }
      else
        interpolated = times[thisPos]->copy();

      // time/value at thisPosition is now done, goto next
      thisPos++;
    }
    // otherPos value should be inserted before thisPos
    // insert the time in the result recordset, and estimate
    // the value that should be added to it for interpolation
    // (it should be between thisPos-1 and thisPos)
    else if (other->times[otherPos]->time < times[thisPos]->time)
    {
      // thisPos > 0 and record.time should be in
      // between time at [thisPos-1] and time at [thisPos], so
      // use records at thisPos-1 and thisPos to estimate the
      // value at records.time so it can be added to
      // records.concentration
      if (thisPos > 0)
      {
        interpolated = interpolateTimePeriod(times[thisPos-1],
          times[thisPos],
          other->times[otherPos]->time);
        interpolated->add(other->times[otherPos]);
      }
      else
        interpolated = other->times[otherPos]->copy();
      // time/value at thisPosition is now done, goto next
      otherPos++;
    }
    // otherwise times are the same, and all that needs to be done
    // is adding the values
    else
    {
      interpolated = times[thisPos]->copy();
      interpolated->add(other->times[otherPos]);

      thisPos++;
      otherPos++;
    }
    resultContam->times[resultContam->numtimes] = interpolated;
    resultContam->numtimes++;
  }

  // if any values are left then push them on to the result records
  // no need to interpolate
  while(thisPos < numtimes)
  {
    interpolated = times[thisPos]->copy();
    resultContam->times[resultContam->numtimes] = interpolated;
    resultContam->numtimes++;
    thisPos++;
  }
  while(otherPos < other->numtimes)
  {
    interpolated = other->times[otherPos]->copy();
    resultContam->times[resultContam->numtimes] = interpolated;
    resultContam->numtimes++;
    otherPos++;
  }

  // add progenies
  if (!isProgeny() && numprogs > 0)
  {
    int i;
    int j;
    bool found;
    bool *added = new bool[numprogs];
    for (i = 0; i < numprogs; i++)
      added[i] = false;
    int count = 0;
    for (j = 0; j < other->numprogs; j++)
    {
      found = false;
      for (i = 0; i < numprogs; i++)
      {
        if (!progs[i])
          continue;
        ATOCon *result = progs[i]->add(other->progs[j]);
        if (result)
        {
          found = true;
          resultContam->progs[count] = result;
          added[i] = true;
          // delete progs[i];
          // progs[i] = 0;
          count++;
          break;
        }
      }
      if (!found)
      {
        resultContam->progs[count] = other->progs[j]->copy();
        count++;
      }
    }

    // copy the leftovers from this progs array
    for (i = 0; i < numprogs; i++)
      if (!added[i])
      {
        resultContam->progs[count] = progs[i]->copy();
        count++;
        // delete [] progs[i];
      }
      resultContam->numprogs = count;
      // delete [] progs;
      delete [] added;
  }
  return resultContam;
}

ATOCon* ATOCon::copy()
{
  ATOCon *resultContam = new ATOCon(isProgeny());
  rstrcpy(resultContam->chemname, chemname);
  rstrcpy(resultContam->casid, casid);
  resultContam->numtimes = numtimes;
  resultContam->numprogs = numprogs;
  resultContam->times = new ATOTimePeriod*[numtimes];
  for (int i = 0; i < resultContam->numtimes; i++)
    resultContam->times[i] = times[i]->copy();
  if (!isProgeny())
  {
    resultContam->progs = new ATOCon*[numprogs];
    for (int i = 0; i < resultContam->numprogs; i++)
      resultContam->progs[i] = progs[i]->copy();
  }
  return resultContam;
}

ATOTimePeriod *ATOCon::interpolateTimePeriod(ATOTimePeriod *timePeriod1,
  ATOTimePeriod *timePeriod2,
  double targetTime)
{
  if (!timePeriod1 || !timePeriod2)
    return 0;

  double time1 = timePeriod1->time;
  double time2 = timePeriod2->time;
  ATOTimePeriod *timePeriod = new ATOTimePeriod();
  timePeriod->time = targetTime;
  rstrcpy(timePeriod->unit, timePeriod1->unit);
  // ATOGrid *grid;

  // go through all grids in timePeriod1.
  // for grids that match grids in timePeriod2, interpolate
  // them and append them to the result timePeriod.
  // for grids that don't match, append only the grid
  // from timePeriod1.  if the grid from timePeriod2 was
  // appended also, then there would be overlap when
  // we go back to append the grids from timePeriod2
  // that weren't in timerPeriod1 (say, if timerPeriod2 had
  // 12 outputs and timePeriod1 had only 4)
  for (int i = 0; i < timePeriod1->getnumoutput(); i++)
  {
    ATOGrid *grid, *grid1 = timePeriod1->grids[i];
    ATOGrid *grid2 = timePeriod2->getGrid(grid1->type,
      grid1->flux->fluxname,
      grid1->moist);

    // if (grid2)
    // make sure both grids match
    // grid = grid1->synchGrids(grid2);

    if (!grid1->synchGrids(grid2))
    {
      // if grid is grid2, then grid2 was already a match to
      // grid1, and no newly created grid needs to be
      // deleted
      // bool deleteWhenDone = (grid != grid2);

      // grid really holds a converted version of grid2, so
      // put it back in that variable for simple sake
      // grid2 = grid;

      // all we really want is the structure of grid2, even
      // though the values will be copied as well.
      grid = new ATOGrid(grid2);

      // linear interpolation is used.  the times are treated
      // as x axis values, and the values at [i][j] are treated
      // as y axis values.
      for (int j = 0; j < grid->axis2num; j++)
        for (int i = 0; i < grid->axis1num; i++)
          grid->values[i][j] = interpolate(time1, grid1->values[i][j],
          time2, grid2->values[i][j], targetTime);
        timePeriod->appendGrid(grid);
        // if (deleteWhenDone)
        // delete grid2;
    }
    else
      timePeriod->appendGrid(new ATOGrid(grid1));
  }

  // add the leftover grids of time period 2
  for (int j = 0; j < timePeriod2->getnumoutput(); j++)
  {
    ATOGrid *grid = timePeriod1->getGrid(timePeriod2->grids[j]->type,
      timePeriod2->grids[j]->flux->fluxname,
      timePeriod2->grids[j]->moist);

    if (!grid)
      timePeriod->appendGrid(timePeriod2->grids[j]);
  }

  return timePeriod;

}

int ATOCon::getTimeSeries(char *product, char *flux, char *moist, double x, double y)
{
  int i, j;
  int gridCount;
  if (numtimes < 1) return numtimes;
  validCount = 0;

  rstrcpy(this->product, product);
  rstrcpy(this->flux, flux);
  rstrcpy(this->moist, moist);
  rstrcpy(this->units, "");
  this->xcoord = x;
  this->ycoord = y;
  if (validTimes != NULL)  {
    delete[] validTimes;
  }
  validTimes = new int[numtimes];

  for (i = 0; i<numtimes; i++){
    gridCount = times[i]->getnumoutput();
    for (j = 0; j<gridCount; j++){
      if (times[i]->getGrid(product, flux, moist) != NULL)
      {
        rstrcpy(units, times[i]->grids[j]->dataunits);
        validTimes[validCount] = i;
        validCount++;
        break;
      }
    }
  }
  return validCount;
}

double ATOCon::getTimeSeriesTime(int n){
  return times[validTimes[n]]->time;
}
double ATOCon::getTimeSeriesValue(int n){
  return times[validTimes[n]]->getGrid(product, flux, moist)->getValue(xcoord, ycoord);
}
void ATOCon::getTimeSeriesXAxisUnit(char *unit){
  rstrcpy(unit, times[validTimes[0]]->unit);
}
void ATOCon::getTimeSeriesYAxisUnit(char *unit){
  rstrcpy(unit, times[validTimes[0]]->getGrid(product, flux, moist)->dataunits);
}

// ------------------------------------------------------------------------------
// class ATOFluxType
ATOFluxType::ATOFluxType()
{
  rstrcpy(fluxfullname, "");
  rstrcpy(oldfluxname, "");
  rstrcpy(fluxname, "");
  rstrcpy(radunits, "");
  rstrcpy(denunits, "");
  rstrcpy(type, "");
  radius = 0.0;
  density = 0.0;
  index = 0;
}

ATOFluxType::~ATOFluxType()
{
  rstrcpy(fluxfullname, "");
  rstrcpy(oldfluxname, "");
  rstrcpy(fluxname, "");
  rstrcpy(radunits, "");
  rstrcpy(denunits, "");
  rstrcpy(type, "");
  radius = 0.0;
  density = 0.0;
  index = 0;
}

bool ATOFluxType::Read(icsv* infile)
{
  *infile >> fluxname >> radius >> radunits >> density >> denunits >> NewLn;
  // parse the flux type
  // assume the first word is the type
  // the rest is index
  rstrcpy(type, fluxname);
  char *c = strchr(type, ' ');
  if (c) c[0] = '\0';
  return true;
}

void ATOFluxType::Write(ocsv* outfile)
{
  *outfile << getFullName() << radius << radunits << density << denunits << NewLn;
}

ATOFluxType *ATOFluxType::copy()
{
  ATOFluxType *flux = new ATOFluxType();
  rstrcpy(flux->oldfluxname, oldfluxname);
  rstrcpy(flux->fluxname, fluxname);
  rstrcpy(flux->radunits, radunits);
  rstrcpy(flux->denunits, denunits);
  rstrcpy(flux->type, type);
  flux->radius = radius;
  flux->density = density;
  flux->index = index;

  return flux;
}

char * ATOFluxType::getFullName()
{
  char buff[SMALLSTRING];

  itoa(index, buff, 10);
  rstrcpy(fluxfullname, type, " ", buff);
  return fluxfullname;
}

// ------------------------------------------------------------------------------
// class ATOSet
ATOSet::ATOSet()
{
  fluxnum = 0;
  numcons = 0;
  rstrcpy(name, "");
  rstrcpy(modname, "");
  rstrcpy(releasetype, "");
  rstrcpy(coordtype, "");
  rstrcpy(spatialtype, "");
  fluxes = NULL;
  cons = NULL;
}

ATOSet::~ATOSet()
{
  int i;
  fluxnum = 0;
  numcons = 0;
  rstrcpy(name, "");
  rstrcpy(modname, "");
  rstrcpy(releasetype, "");
  rstrcpy(coordtype, "");
  rstrcpy(spatialtype, "");
  for (i = 0; i<fluxnum && fluxes; i++)
    delete fluxes[i];
  delete fluxes;
  for (i = 0; i<numcons && cons; i++)
    delete cons[i];
  delete cons;
}

bool ATOSet::Read(icsv* infile)
{
  int i, numflux = 0;
  *infile >> numflux >> name >> NewLn;
  // fluxes = new ATOFluxType*[numflux];
  for (i = 0; i<numflux; i++)
  {
    ATOFluxType *flux = new ATOFluxType();
    flux->Read(infile);
    addFlux(flux);
    // this->addFlux(new ATOFluxType());
    // fluxes[i]->Read(infile);
    ::addFlux(flux);
  }
  // make this dataset known to children (such as grids).   this is ok so long
  // as this function is called in a single threaded process
  currentDataset = this;

  *infile >> releasetype >> coordtype >> spatialtype >> numcons;
  if (!rstrcmpi(releasetype,"acute"))
    *infile >> yr >> mon >> day >> hr >> min;
  *infile >> NewLn;
  if (!rstrcmpi(coordtype, "polar")) polartype = true;
  else polartype = false;
  if (!rstrncmpi("grid", spatialtype, 4)) gridtype = true;
  else gridtype = false;
  cons = new ATOCon*[numcons];
  for (i = 0; i<numcons; i++)
    cons[i] = new ATOCon();
  for (i = 0; i<numcons; i++)
    cons[i]->Read(infile);

  currentDataset = 0;
  return true;
}

void ATOSet::Write(ocsv *outfile)
{
  int i;
  *outfile << fluxnum << name << NewLn;
  for (i = 0; i<fluxnum; i++)
    fluxes[i]->Write(outfile);
  *outfile << releasetype;
  if (!rstrcmpi(workingtype, "cartesian") ||
      !rstrcmpi(workingtype, "polar"))
    *outfile << workingtype;
  else
    *outfile << coordtype;
  if (writelinear)
    *outfile << "points";
  else
    *outfile << "grid";

  *outfile << numcons << NewLn;

  for (i = 0; i<numcons; i++)
    cons[i]->Write(outfile);
}

bool ATOSet::add(ATOSet *other)
{
  int i;
  // verify types can be added
  if (rstrcmpi(releasetype, other->releasetype) != 0)
    return false;

  // by now, fluxes in this dataset and fluxes in other dataset
  // should have globalized names
  for (i = 0; i < other->fluxnum; i++)
  {
    // copy the flux
    ATOFluxType *newflux = other->fluxes[i]->copy();
    // if the flux already exists in this dataset, delete the copy
    // otherwise add it
    if (!addFlux(newflux))
      delete newflux;
  }
  ATOCon **newcons = new ATOCon*[numcons + other->numcons];

  bool found;
  int count = 0;
  for (int j = 0; j < other->numcons; j++)
  {
    found = false;
    for (int i = 0; i < numcons; i++)
    {
      if (!cons[i])
        continue;
      ATOCon *result = cons[i]->add(other->cons[j]);
      if (result)
      {
        found = true;
        newcons[count] = result;
        delete cons[i];
        cons[i] = 0;
        count++;
        break;
      }
    }
    if (!found)
    {
      newcons[count] = other->cons[j]->copy();
      count++;
    }
  }

  // copy the leftovers from this cons array
  for (i = 0; i < numcons; i++)
    if (cons[i])
    {
      newcons[count] = cons[i]->copy();
      count++;
      delete cons[i];
    }
    delete [] cons;
    cons = newcons;
    return true;

}

ATOFluxType *ATOSet::getFlux(char *oldfluxname)
{
  // search for flux with fluxname
  for (int i = 0; i < fluxnum; i++)
  {
    if (!rstrcmpi(trim(oldfluxname), fluxes[i]->oldfluxname))
      return fluxes[i];
  }

  return 0;
}

bool ATOSet::addFlux(ATOFluxType *flux)
{
  int i;

  if (!fluxes)
  {
    fluxes = new ATOFluxType*[1];
    fluxes[0] = flux;
    fluxnum++;
    return true;
  }

  // by this time, reads of relevant datasets have
  // already been called, and flux names have
  // been globalized, therefore it is ok to compare
  // fluxtypes by name instead of properties.
  for (i = 0; i < fluxnum; i++)
  {
    if (!rstrcmpi(flux->fluxname, fluxes[i]->fluxname))
      return false;
  }

  // add flux
  ATOFluxType **temp = fluxes;
  fluxes = new ATOFluxType*[fluxnum + 1];

  // copy over the existing fluxes
  for (i = 0; i < fluxnum; i ++)
    fluxes[i] = temp[i];

  // add new flux
  fluxes[i] = flux;
  fluxnum++;

  // delete old flux array
  delete [] temp;

  return true;
}

int ATOSet::convertATO(ocsv *eOut, char *casList)
{
  int i, j, p;
  ATOCon *con;
  ATOCon *tmp;
  ATOCon *result;
  bool plus;
  bool found;

  char *mylist = strdup(casList);
  splitList(mylist);
  // dataset info
  *eOut << fluxnum << name << NewLn;

  for (i = 0; i<fluxnum; i++)
    fluxes[i]->Write(eOut);
  *eOut << releasetype;
  if (!rstrcmpi(workingtype, "cartesian") ||
      !rstrcmpi(workingtype, "polar"))
    *eOut << workingtype;
  else
    *eOut << coordtype;
  if (writelinear)
    *eOut << "points";
  else
    *eOut << "grid";

  *eOut << (long)rList.size();
  if (!rstrcmpi(releasetype,"acute"))
    *eOut << yr << mon << day << hr << min;
  *eOut << NewLn;

  for (p = 0; p< (long)rList.size(); p++)
  {
    // loading chemical names
    tmp = NULL;
    plus = false;
    for (i = 0; i<numcons; i++)
    {
      con = NULL;
      found = !rstrcmpi(rList[p], cons[i]->casid);
      if (found)
        con = cons[i];
      else
        for (j = 0; j<cons[i]->numprogs; j++)
        {
          con = NULL;
          found = !rstrcmpi(rList[p], cons[i]->progs[j]->casid);
          if (found)
          {
            con = cons[i]->progs[j];
            break;
          }
        }

      if (found)
      {
        if (tmp)
        {
          bool isp = tmp->isprogeny;
          tmp->isprogeny = false;
          result = tmp->add(con);
          tmp->isprogeny = isp;
          if (result)
          {
            if (plus) delete tmp;
            tmp = result;
            plus = true;
          }
        }
        else
          tmp = con;
      }
    }

    if (tmp == NULL)
      *eOut << "Not found" << rList[p] << "yr" << "?" << 0 << 0 << NewLn;
    else
    {
      ClearDecayFlags();
      GetDecayChain(p);
      i = tmp->numprogs;
      tmp->numprogs = 0;
      tmp->Write(eOut, false);
      tmp->numprogs = i;
    }
    if (plus) delete tmp;
  }

  delete mylist;
  return 0;
}

// this routine will add liked named parents in the caslist
// then write them with degradation product that have zero time steps
// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
int ATOSet::unconvertATO(ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i, j, p, t, last, cnt;
  ATOCon *con;
  ATOCon *tmp;
  ATOCon *result;
  bool plus;
  bool found;

  cnt = 0;
  OpenDecayChain(casList, nameList, degList, secList, branching);

  // dataset info
  *eOut << fluxnum << name << NewLn;

  for (i = 0; i<fluxnum; i++)
    fluxes[i]->Write(eOut);

  *eOut << releasetype;
  if (!rstrcmpi(workingtype, "cartesian") ||
      !rstrcmpi(workingtype, "polar"))
    *eOut << workingtype;
  else
    *eOut << coordtype;
  if (writelinear)
    *eOut << "points";
  else
    *eOut << "grid";

// need to fix up consitutent count
//  *eOut << (long)cList.size();
  long fpos = eOut->getpos();
  fprintf(eOut->fptr,",     ");

  if (!rstrcmpi(releasetype,"acute"))
    *eOut << yr << mon << day << hr << min;
  *eOut << NewLn;

  for (p = 0; p< (long)cList.size(); p++)
  {
    // loading chemical names
    tmp = NULL;
    plus = false;
    for (i = 0; i<numcons; i++)
    {
      con = NULL;
      found = !rstrcmpi(cList[p], cons[i]->casid);
      if (found)
        con = cons[i];
      else
        for (j = 0; j<cons[i]->numprogs; j++)
        {
          con = NULL;
          found = !rstrcmpi(cList[p], cons[i]->progs[j]->casid);
          if (found)
          {
            con = cons[i]->progs[j];
            break;
          }
        }
      if (found)
      {
        if (tmp)
        {
          bool isp = tmp->isprogeny;
          tmp->isprogeny = false;
          result = tmp->add(con);
          tmp->isprogeny = isp;
          if (result)
          {
            // prevents deleteing from cons
            if (plus) delete tmp;
            tmp = result;
            plus = true;
          }
        }
        else
          tmp = con;
      }
    }

    if (tmp == NULL)
      printf("Not found %s" , cList[p]);
    else
    {
      if (tmp->numtimes <=0)
        printf("No timeperiods for %s" , cList[p]);
      else
      {
        if (tmp->times[0]->getnumoutput() <= 0)
           printf("No outputs for %s" , cList[p]);
        else
        {
          cnt++;
          ClearDecayFlags();
          GetDecayChain(p);

          i = tmp->numprogs;
          tmp->numprogs = (long)d3List.size();
          tmp->Write(eOut, false);
          tmp->numprogs = i;

          // write out progeny list with no time pts
          last = p;
          for (i = 0; i< (long)d3List.size(); i++)
            for (j = 0; j< (long)cList.size(); j++)
              if (!rstrcmpi(d3List[i], cList[j]))
              {
                *eOut << nList[j] << cList[j] << tmp->numtimes << nList[last] << cList[last] << NewLn;
                for (t=0; t< tmp->numtimes; t++)
                  *eOut << tmp->times[t]->time << tmp->times[t]->unit << 0 << NewLn;;
                last = j;
                break;
              }
        }
      }
    }
    if (plus) delete tmp;
  }

  long lpos = eOut->getpos();
  eOut->setpos(fpos);
  *eOut << cnt;
  eOut->setpos(lpos);

  CloseDecayChain();
  return 1;
}

// ------------------------------------------------------------------------------
// class ATO
void ATO::Init()
{
  inf = NULL;
  set = NULL;
  numSet = 0;
}

ATO::ATO()
{
  Init();
}

ATO::ATO(char *gridtype)
{

  Init();

  // set the type of grid to work with and output
  // specify "polar" or "cartesian"
  if (gridtype)
    rstrcpy(workingtype, gridtype);
  else
    rstrcpy(workingtype, "");
}

ATO::~ATO()
{
  if (set)
  {
    for (int i = 0; i<numSet; i++)
      if (set[i]) delete set[i];
      delete[] set;
  }
  if (inf) delete inf;
  Init();
}

bool ATO::ChangeName(int dsIdx, char* dsName)
{
  if (dsName != NULL && dsIdx < numSet && dsIdx > -1)
  {
    rstrcpy(set[dsIdx]->name, dsName);
    return true;
  }
  return false;
}

bool ATO::Read(char *fuiname, char *modId, int x, int y, int z)
{
  char filename[MAXPATH];

  g_x = x;
  g_y = y;
  g_z = z;

  rstrcpy(name, modId);
  sprintf(filename, "%s.ato", fuiname);
  inf = new icsv(filename);
  if (!inf->ok()) return false;
  if (0 != inf->SeekSection(modId, &atoHead)) return false;
  *inf >> numSet >> NewLn;
  set = new ATOSet*[numSet];
  for (int i = 0; i<numSet; i++)
  {
    set[i] = new ATOSet();
    set[i]->Read(inf);
  }
  return true;
}

void ATO::Write(char* filename, bool linear)
{
  int i;

  writelinear = linear;
  ocsv outFile(filename);
  outFile.alwaysQuote();
  // output 0 for header line length (also output number datasets)
  outFile << 1 << NewLn << "File created with ATO I/O tools" << NewLn;
  outFile << numSet << NewLn;
  for (i = 0; i < numSet; i++)
    set[i]->Write(&outFile);
}

bool ATO::add(ATO *other)
{
  return set[0]->add(other->set[0]);
}

// ------------------------------------------------------------------------------
// ATO API ---------------------------------------------------------------------
// ------------------------------------------------------------------------------
void FRAMES_API atoOpen(char *fuiname, char *modId)
{
  ato = new ATO();
  ato->Read(fuiname, modId);
}

void FRAMES_API atoClose()
{
  if (ato) delete ato;
  ato = NULL;
}

int FRAMES_API atoGetNumRunInfo()
{
  int num;
  if (ato == NULL) return 0;
  if (ato->numSet<= 0) return 0;
  ato->inf->setpos(ato->atoHead);
  *ato->inf >> num >> NewLn;
  return num;
}

int FRAMES_API atoGetRunInfo(int Idx, char *info)
{
  int num = atoGetNumRunInfo();
  if (num == 0 || Idx>num) return 0;
  for (int i = 1; i<Idx; i++)
    *ato->inf >> NewLn;
  *ato->inf >> info >> NewLn;
  return 1;
}
int FRAMES_API atoGetDatasetCount(){
  return ato->numSet;
}
int FRAMES_API atoGetConstituentCount(int dsIdx){
  return ato->set[dsIdx]->numcons;
}
int FRAMES_API atoGetNumFluxCount(int dsIdx){
  return ato->set[dsIdx]->fluxnum;
}
void FRAMES_API atoGetDatasetName(int dsIdx, char *name){
  rstrcpy(name, ato->set[dsIdx]->name);
}
void FRAMES_API atoGetDatasetModname(int dsIdx, char *name){
  rstrcpy(name, ato->set[dsIdx]->modname);
}
void FRAMES_API  atoGetCoordinateType(int dsIdx, char *name){
  rstrcpy(name, ato->set[dsIdx]->coordtype);
}
void FRAMES_API  atoGetReleaseType(int dsIdx, char *name){
  rstrcpy(name, ato->set[dsIdx]->releasetype);
}
void FRAMES_API  atoGetSpatialType(int dsIdx, char *name){
  rstrcpy(name, ato->set[dsIdx]->spatialtype);
}
// ----------ATOFlux calls -------------------
void FRAMES_API atoGetFluxType(int dsIdx, int fluxIndex, char* name){
  rstrcpy(name, ato->set[dsIdx]->fluxes[fluxIndex]->oldfluxname);
}
double FRAMES_API atoGetFluxRadius(int dsIdx, int fluxIndex){
  return ato->set[dsIdx]->fluxes[fluxIndex]->radius;
}
void FRAMES_API atoGetFluxRadiusUnits(int dsIdx, int fluxIndex, char *name){
  rstrcpy(name, ato->set[dsIdx]->fluxes[fluxIndex]->radunits);
}
double FRAMES_API atoGetFluxDensity(int dsIdx, int fluxIndex){
  return ato->set[dsIdx]->fluxes[fluxIndex]->density;
}
void FRAMES_API atoGetFluxDensityUnits(int dsIdx, int fluxIndex, char *name){
  rstrcpy(name, ato->set[dsIdx]->fluxes[fluxIndex]->denunits);
}
// -----------constituent calls --------------
int FRAMES_API atoGetProgenyCount(int dsIdx, int constituentID){
  return ato->set[dsIdx]->cons[constituentID]->numprogs;
}
void FRAMES_API atoGetChemName(int dsIdx, int constituentID, int progIdx, char * name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[constituentID]->chemname);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[constituentID]->progs[progIdx]->chemname);
}
void FRAMES_API atoGetCasID(int dsIdx, int constituentID, int progIdx, char * name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[constituentID]->casid);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[constituentID]->progs[progIdx]->casid);
}
// ----------Progeny calls --------------------
void FRAMES_API atoGetParentCasID(int dsIdx, int conIdx, int progIdx, char* name){
  rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->parencasid);
}
void FRAMES_API atoGetParentChemName(int dsIdx, int conIdx, int progIdx, char* name){
  rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->parenname);
}
int FRAMES_API atoGetTimePeriodCount(int dsIdx, int conIdx, int progIdx){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->numtimes;
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->numtimes;
}
// ---------TimePeriod calls ------------------
int FRAMES_API atoGetTimePeriodOutputCount(int dsIdx, int conIdx, int progIdx, int timeIdx){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->getnumoutput();
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->getnumoutput();
}
void FRAMES_API atoGetTimePeriodUnit(int dsIdx, int conIdx, int progIdx, int timeIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->unit);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->unit);
}
double FRAMES_API atoGetTimePeriodTime(int dsIdx, int conIdx, int progIdx, int timeIdx){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->time;
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->time;
}
// --------Grid calls -------------------------
int FRAMES_API atoGetGridAxis1Count(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis1num;
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis1num;
}
int FRAMES_API atoGetGridAxis2Count(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis2num;
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis2num;
}
double FRAMES_API atoGetGridAxis1Value(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, int index){
  if (progIdx<0)
    return (double)ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis1values[index];
  else
    return (double)ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis1values[index];
}
double FRAMES_API atoGetGridAxis2Value(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, int index){
  if (progIdx<0)
    return (double)ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis2values[index];
  else
    return (double)ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis2values[index];
}
void FRAMES_API atoGetGridAxis1Unit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis1units);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis1units);
}
void FRAMES_API atoGetGridAxis2Unit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->axis2units);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->axis2units);
}
void FRAMES_API atoGetGridDataUnit(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->dataunits);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->dataunits);
}
void FRAMES_API atoGetGridMoist(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->moist);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->moist);
}
int FRAMES_API atoGetGridFluxIndex(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx){
  if (progIdx<0)
    for (int i = 0; i< ato->set[dsIdx]->fluxnum; i++)
    {
      if (ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->flux == ato->set[dsIdx]->fluxes[i])
        return i;
    }
    else
      for (int i = 0; i< ato->set[dsIdx]->fluxnum; i++)
      {
        if (ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->flux == ato->set[dsIdx]->fluxes[i])
          return i;
      }
  return -1;
}
void FRAMES_API atoGetGridOutputType(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, char* name){
  if (progIdx<0)
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->type);
  else
    rstrcpy(name, ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->type);
}
double FRAMES_API atoGetGridMax(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, double& xCoord, double& yCoord){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->GetGridMax(xCoord, yCoord);
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->GetGridMax(xCoord, yCoord);
}
double FRAMES_API atoGetGridMin(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, double& xCoord, double& yCoord){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->GetGridMin(xCoord, yCoord);
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->GetGridMin(xCoord, yCoord);
}
double FRAMES_API atoGetGridValue(int dsIdx, int conIdx, int progIdx, int timeIdx, int gridIdx, int axis1, int axis2)
{
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->times[timeIdx]->grids[gridIdx]->values[axis1][axis2];
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->times[timeIdx]->grids[gridIdx]->values[axis1][axis2];
}
int FRAMES_API atoGetTimeSeries(int dsIdx, int conIdx, int progIdx, char *product, char *flux, char *moist, double x, double y){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->getTimeSeries(product, flux, moist, x, y);
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->getTimeSeries(product, flux, moist, x, y);
}
double FRAMES_API atoGetTimeSeriesTime(int dsIdx, int conIdx, int progIdx, int n){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->getTimeSeriesTime(n);
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->getTimeSeriesTime(n);
}
double FRAMES_API atoGetTimeSeriesValue(int dsIdx, int conIdx, int progIdx, int n){
  if (progIdx<0)
    return ato->set[dsIdx]->cons[conIdx]->getTimeSeriesValue(n);
  else
    return ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->getTimeSeriesValue(n);
}
void FRAMES_API atoGetTimeSeriesXAxisUnit(int dsIdx, int conIdx, int progIdx, char *unit){
  if (progIdx<0)
    ato->set[dsIdx]->cons[conIdx]->getTimeSeriesXAxisUnit(unit);
  else
    ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->getTimeSeriesXAxisUnit(unit);
}
void FRAMES_API atoGetTimeSeriesYAxisUnit(int dsIdx, int conIdx, int progIdx, char *unit){
  if (progIdx<0)
    ato->set[dsIdx]->cons[conIdx]->getTimeSeriesYAxisUnit(unit);
  else
    ato->set[dsIdx]->cons[conIdx]->progs[progIdx]->getTimeSeriesYAxisUnit(unit);
}

void FRAMES_API atoAggregate(char *filename, char *casList)
{
  int i;
  char *dummy = new char[MAXPATH];

  if (ato == NULL) return;
  ocsv *eOut = new ocsv(filename, '"', ',' , _CREATE_);
  eOut->alwaysQuote();
  *eOut << (atoGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /out" << NewLn;
  for (i = 0; i<atoGetNumRunInfo(); i++)
  {
    atoGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << ato->numSet << NewLn;
  for (i = 0; i<ato->numSet; i++)
    ato->set[i]->convertATO(eOut, casList);

  delete eOut;
  delete [] dummy;
  return;
}

void FRAMES_API atoInsert(char *filename, char *casList, char *nameList, char *degList, char *secList, int branching)
{
  int i;
  long pos;
  char *dummy = new char[MAXPATH];

  if (ato == NULL) return;
  ocsv *eOut;
  if (0 == access(filename, 0))
    eOut = new ocsv(filename, '"', ',', _APPEND_);
  else
    eOut = new ocsv(filename, '"', ',', _CREATE_);
  eOut->alwaysQuote();
  *eOut << ato->name;
  pos = eOut->getpos();
  eOut->smartQuote();
  *eOut << "          " << NewLn;
  eOut->alwaysQuote();
  *eOut << (atoGetNumRunInfo()+1) << NewLn;
  *eOut << "This file was modified by wrapspec.exe /in" << NewLn;
  for (i = 0; i<atoGetNumRunInfo(); i++)
  {
    atoGetRunInfo(i+1, dummy);
    *eOut << dummy << NewLn;
  }
  *eOut << ato->numSet << NewLn;
  for (i = 0; i<ato->numSet; i++)
    ato->set[i]->unconvertATO(eOut, casList, nameList, degList, secList, branching);

  eOut->setpos(pos);
  eOut->delim = ' ';
  *eOut << ((eOut->lnct)-1);

  delete eOut;
  delete [] dummy;
  return;
}

