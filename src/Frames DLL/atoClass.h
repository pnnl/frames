/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________

    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef atoClassH
#define atoClassH

//---------------------------------------------------------------------------

#include "csv.h"

void clearfluxlist();
bool convertToPolar(double &x, double &y);
bool convertFromPolar(double &radius, double &angle);
bool convertToCompass(double &x, double &y);
bool convertFromCompass(double &radius, double &angle);

class ATOFluxType;

class ATOGrid
{
public:
  int axis1num;
  int axis2num;
  char dataunits[SMALLSTRING];
  char axis1units[SMALLSTRING];
  char axis2units[SMALLSTRING];
  char moist[SMALLSTRING];
  char type[SMALLSTRING];

  ATOFluxType *flux;
  double *axis1values;
  double *axis2values;
  double **values;

  // a variable used to notify fuctions of this class that
  // the input was linear.  for most function calls it won't
  // matter because it will have been converted to grid
  // by the time they are called.  but some functions,
  // such as closestThree and convert, are helper functions
  // in converting from linear to grid, and need to know
  // that input is currently in linear format, so that they
  // don't treat it as grid and access wrong parts of memory
  bool linear;

  ATOGrid();
  ATOGrid(ATOGrid* grid);
  ~ATOGrid();
  bool Read(icsv* infile);
  void Write(ocsv* outfile, bool writelinear = false);
  double GetGridMax(double &xcoord,double &ycoord);
  double GetGridMin(double &xcoord,double &ycoord);
  double getValue(double xcoord, double ycoord);
  int getAxis1index(double xcoord);
  int getAxis2index(double ycoord);
  ATOGrid* ZoomGridX2(void);

  /*
  * compares the units, moist, and flux types of grids.  if they
  * all match, returns true, else false
  */
  bool matches(ATOGrid* other);
  /*
  * returns a new grid, matching this grid, based on the grid
  * values passed in the "other" parameter
  * will return grid "other" if they already match (are synched).
  * will return 0 if an error occurs.
  * by match, i men that matches(other) will return true,
  * and that both grids have the same axis header values
  * and in the same order.  if the criteria under matches(other)
  * fails, there is no way to convert grid "other" to synch up with
  * this one.
  */
  bool synchGrids(ATOGrid* other);
  /*
  * compares the axis headers of both grids (this and other).
  * each axis must have the same count of values, and each
  * of those values at the same index must be equivalent.
  */
  bool compareAxis(ATOGrid *other);
  /*
  * first verifies that this and other grid can be added (they
  * are of the same type and hold same axis values).  if the
  * types don't match, false is returned.  if the axis values
  * don't match, then a new interpolated grid is created
  * based on the other grid, with matching header values
  * to this grid.  in other words, it aligns the other grid with
  * this grid, so that they can be added.  the other grid
  * is not modified when this function exits.
  */
  bool add(ATOGrid* other);
  /*
  * returns the bounds of this grid.  return units will be that of type
  * axis1units for x, and axis1units for y, if type is null.
  * the type argument is useful for specifying which bounds you want to
  * get.  set type = "cartesian" if you want to return the bounding
  * box of all the points (thus the min and max returns will be in
  * cartesian, even if the grid is currently of type polar).  set type
  * to "polar" if you want a polar range surrounding all points.  a
  * polar range radius (specified by minX, maxX) will be between 0 and
  * n.  a polar degree range (specified by minY, maxY) will be equal to
  * or between 0 and 360.
  */
  void getBounds(double &minX, double &minY, double &maxX, double &maxY, const char *type=0);
  /**
  * returns true if polar, false if cartesian.  determines by looking at the
  * axis2units variable.  if it's deg then polar is true, else false (and thus
  * assumed to be cartesian).
  */
  bool isPolar();
  /**
  * convert the current grid.  sizeX and sizeY specify the new axis dimensions.
  * listX and listY specify new axis1 and axis 2 values, if desired.  if listX and
  * listY are null, sizeX and sizeY will be used to create new axis headers
  * of these sizes, based off existing values. (ie it will become a sizeX by sixeY
  * grid).
  * the type variable specifies the new grid type.  options are "polar",
  * "cartesian", and 0.   if 0, the grid dimension conversion will keep values
  * in the same units.
  * the first four parameters can be left to 0,and the last parameter can
  * be of "polar" or "cartesian" to convert a polar grid to a cartesian grid,
  * and vise versa.
  */
  void convert(int sizeX, double *listX, int sizeY, double *listY, const char *type=0);
  /**
  * after read is called, this function is usually called directly after to turn the
  * linear input into a grid.  the axis values are estimated based on the input
  * linear coordinates to form a uniform grid.
  */
  void linearToGrid();
  /**
  * gets the closest three points to (x, y).  stores them in listX, listY.
  * if the actual point matching x, y is in the grid, this function returns
  * true, and the values in listX and listY may be invalid because the
  * function terminated early.  note that the grid should be bigger than 4
  * elements, so that (listX, listY) can be filled with proper values.
  * indexes are stored in listX, listY because the axis values can be
  * retrieved as fast as the value at those axis values (instead of calling
  * getvalue() with axis values).
  * the values x and y should be of the same units as axis1units and axis2units
  * respectivly.  closestThree will make appropriate conversions.
  */
  bool closestThree(double x, double y, int* listX, int* listY);
  /**
  * a grid origin is assumed to be at 0,0.  with this function
  * you can specify the origin of the grid (offset it from (0,0)).
  */
  void offset(int x, int y);
};
//---------------------------------------------------------------------------
const int STEP_SIZE = 10;
class ATOTimePeriod
{
private:
  /*
    made numoutput private on purpose, because it should
    never have public write priveledges.
    the appendGrid takes care of the numoutput variable
    if desired the getnumouput function can be used to get the value of it.
  */
  int numoutput;

public:
  double time;
  char unit[SMALLSTRING];
  ATOGrid **grids;

  ATOTimePeriod();
  ~ATOTimePeriod();
  double getValue(int gridID, double axis1value, double axis2value);
  ATOGrid *getGrid(char *product, char *flux, char *moist);
  ATOGrid *getGrid(int gridIdx);
  bool Read(icsv* infile);
  void Write(ocsv* outfile);
  // convinience function.  resizes the array and tacks on the grid
  void appendGrid(ATOGrid *grid);

  /**
  * first compares time and unit to assure the grids match up.
  * next searches other time period for grids matching in this
  * time period.  if they match, they are added.
  */
  bool add(ATOTimePeriod *other);
  /**
  * return a copy of this class, it's up to the user to delete
  * it.
  */
  ATOTimePeriod *copy();

  int getnumoutput() {return numoutput;}
};
//---------------------------------------------------------------------------
class ATOCon
{
private:
  int validCount;
  int *validTimes;

public:
  bool isprogeny;
  int numtimes;
  int numprogs;
  double xcoord;
  double ycoord;
  char product[SMALLSTRING];
  char flux[SMALLSTRING];
  char moist[SMALLSTRING];
  char units[SMALLSTRING];
  char chemname[SMALLSTRING];
  char casid[SMALLSTRING];
  char parenname[SMALLSTRING];
  char parencasid[SMALLSTRING];
  ATOTimePeriod **times;
  ATOCon **progs;

  ATOCon(bool progeny = false);
  ~ATOCon();
  int getTimeSeries(char *product, char *flux, char *moist, double x, double y);
  double getTimeSeriesTime(int n);
  double getTimeSeriesValue(int n);
  void getTimeSeriesXAxisUnit(char *unit);
  void getTimeSeriesYAxisUnit(char *unit);
  bool Read(icsv* infile);
  void Write(ocsv* outfile, bool writeprog=true);
  ATOCon* copy();
  ATOCon* add(ATOCon *other);
  /**
  * provide two time periods.  return an interpolated time period at
  * time targetTime in between timePeriod1 and timePeriod2
  */
  ATOTimePeriod *interpolateTimePeriod(ATOTimePeriod *timePeriod1,
                                       ATOTimePeriod *timePeriod2,
                                       double targetTime);
    /**
    * used for this class to know if it should behave as a parent
    * consituent or a progeny.
  */
  bool isProgeny() { return isprogeny; }

};
//---------------------------------------------------------------------------
class ATOFluxType
{
private:
  char fluxfullname[SMALLSTRING];

public:
  ///index of flux of type
  int index;
  double radius;
  double density;
  char radunits[SMALLSTRING];
  char denunits[SMALLSTRING];
  char fluxname[SMALLSTRING];
  char oldfluxname[SMALLSTRING];
  char type[SMALLSTRING];

  ATOFluxType();
  ~ATOFluxType();
  bool Read(icsv* infile);
  void Write(ocsv* outfile);
  /**
  * returns an exact copy of this flux type.
  * it is up to user to delete it when done with it.
  */
  ATOFluxType *copy();
  /**
  * returns the full name of the flux, that is
  *  flux->type + " " + flux->index  (i.e. Gas 1)
  */
  char *getFullName();
};
//---------------------------------------------------------------------------
class ATOSet
{
public:
  int fluxnum;
  int numcons;
  int yr;
  int mon;
  int day;
  int hr;
  int min;
  char name[SMALLSTRING];
  char modname[SMALLSTRING];
  char coordtype[SMALLSTRING];
  char spatialtype[SMALLSTRING];
  char releasetype[SMALLSTRING];
  ATOFluxType **fluxes;
  ATOCon **cons;

  ATOSet();
  ~ATOSet();
  bool Read(icsv* infile);
  void Write(ocsv* outfile);
  /**
  * take another dataset and merge it with this
  * one.  constituents, time periods, and grids will
  * all be interpolated, and the result will be stored
  * in this dataset.
  */
  bool add(ATOSet *other);
  /**
  * returns the flux with the new flux name, based
  * on the old flux name.  the old flux name is the
  * one read from the file, and the new flux name
  * is the one created if the old flux name conflicted
  * with any other of the global flux names.  if
  * there was no confliction, then oldfluxname
  * should be the same as the new flux name.
  */
  ATOFluxType *getFlux(char *oldfluxname);
  /**
  * similar to the global addFlux function, except
  * that no flux merging is done.  this flux is just
  * appended to an member list of this object.
  *
  * returns true if flux type was added, false if not
  * (most likly cause it was already in there)
  */
  bool addFlux(ATOFluxType *flux);

  int convertATO(ocsv *eOut, char *casList);
  int unconvertATO(ocsv *eOut, char *casList, char *nameList, char *degList, char *secList, int branching);
};
//---------------------------------------------------------------------------
class ATO
{
public:
  icsv *inf;
  int numSet;
  long atoHead;
  char name[SMALLSTRING];
  ATOSet **set;
  /*
   specify "polar" or "cartesian" to set the default
   internal work type for grids.  set this to whatever
   type you want output when Write() is called.
  */
  ATO();
  ATO(char *gridtype);
  ~ATO();
  void Init();
  bool Read(char* filename, char* ID, int x=0, int y=0, int z=0);
  bool ChangeName(int dsIdx, char* dsName);
  /*
   if linear is true, output is spilled linearly as points instead of as a grid.
  */
  void Write(char* filename, bool linear = false);
  bool add(ATO *other);
};
//---------------------------------------------------------------------------


#endif

