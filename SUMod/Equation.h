#ifndef EQUATION_H
#define EQUATION_H

#include <map>
#include <string>

using namespace std;

typedef map<string,float> ValueMap;

bool eval(string alias,string validx,string equation,ValueMap &data,char **line1,char **line2,char **line3);
bool evalPassedTest();

#endif
