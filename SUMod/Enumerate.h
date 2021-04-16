#ifndef ENUMERATE_H
#define ENUMERATE_H

#include <vector>
#include <string>

using namespace std;

typedef vector<int> intVector;
typedef vector<intVector> intMatrix;
typedef vector<intVector::iterator> indexState;
typedef intVector::iterator intVectorIt;
typedef intMatrix::iterator intMatrixIt;

class Enumerate
{
  bool isDone;
  indexState state;
  intMatrix enumerate;

  intVector parse(string s);
  void helperNext(unsigned i);
 public:
  int size();
  int size(unsigned i);
  int idx(unsigned dim);
  int idx(unsigned i, unsigned j);
  bool done();
  void next();
  void start();
  void debug();
  void insert(intVector v);
  void remove(unsigned i);
  void clear();
  void parseAllIndices(vector<string> indices);

  static bool test1();
  static bool passedTest();
};

#endif