#ifndef PLUS_H
#define PLUS_H

#include <iostream>
#include "gid.h"

#undef OUT
#define OUT cout
#define SAME 0

class model
{
public:

  model()
  {
    strcpy(name, "");
    x = y = z = 0;
  }
  char name[SMALLSTRING];
  int x, y, z;
};

///global variables
extern char readPath[MAXPATH];
extern char writePath[MAXPATH];
extern char modelName[SMALLSTRING];
extern char modelType[SMALLSTRING];
extern int siteIndex;
extern int modelIndex;

/**
 * ouput of the form MESSAGE: here is a message
 */
void outputMessage(const char *message);

/**
 * outputs arguments to the screen, mainly for
 * debugging.
 */
void outputArguments(int argc, char ** argv);

/**
 * validate and parse program arguments, returns
 * false if invalid count of arguments or the
 * type switch is unrecognized.
 */
bool parseArguments(int argc, char **argv);

/**
 * using modelName, inputType, and index, get the name of an input model
 * of type inputType at index.  if index is invalid, or does not contain
 * a position of an input source, or the type of the source model does
 * not match inputType, or some other error occurs, null will be returned.
 * it is up to the user to delete the returned model;
 * 
 * a default index of 0 returns the first index (first model connected).
 * a default inputType of 0 will return any model at index regardless of
 * its type.
 */
model getInput(GIDfile &gid, const char *modelName, int index=0, const char *inputType=0);

/**
  * dummy pause the terminal
  */
void pause();

#endif