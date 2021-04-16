#include "Enumerate.h"
#include "Assertions.h"
#include <iostream>

  int Enumerate::size(unsigned i)
  { return enumerate[i].size(); }

  int Enumerate::idx(unsigned i, unsigned j)
  { return enumerate[i][j];   }

  intVector Enumerate::parse(string s)
  {
    intVector v;
    string::iterator it;
    int e=0,b=-1,i;
    bool atLeastOne=false;
    if (0==strcmpi("all",s.c_str()))
    {
      v.insert(v.end(),-1); // flag 'all'
      return v;
    }
    for(it=s.begin();it!=s.end();++it)
      if (*it>='0' && *it<='9')
      {
        e=e*10+(*it-'0');
        atLeastOne=true;
      }
      else if (*it==',')
      {
        if (b>=0)
          for (i=b;i<=e;i++)
            v.insert(v.end(),i);
        else
          v.insert(v.end(),e);
        b=-1;
        e=0;
      }
      else if (*it=='-')
      {
        b=e;
        e=0;
      }
    if (atLeastOne)
    {
      if (b>=0)
        for (i=b;i<=e;i++)
          v.insert(v.end(),i);
      else
        v.insert(v.end(),e);
    }
    return v;
  }
  void Enumerate::helperNext(unsigned i)
  {
    assert(i>=0 && i<state.size());
    state[i]++;
    if (state[i]==enumerate[i].end())
    {
      state[i]=enumerate[i].begin();
      if (i>0) helperNext(i-1);
      else isDone=true;
    }
  }
  void Enumerate::next()
  {
    if (state.size()==0)
      isDone=true;
    else
     helperNext(state.size()-1);
  }
  int Enumerate::size()
  {
    return state.size();
  }
  void Enumerate::clear()
  { enumerate.clear(); }
  
  void Enumerate::start()
  {
    isDone=false;
    state.clear();
    for (unsigned i=0;i<enumerate.size();i++)
      state.insert(state.end(),enumerate[i].begin());
  }
  int Enumerate::idx(unsigned dim)
  {
    assert(dim>=0 && dim<state.size());
    return *(state[dim]);
  }
  bool Enumerate::done()
  {
    return isDone;
  }

  void Enumerate::debug()
  {
    return;
    int ctx = enumerate.size();
    for (int i=0;i<ctx;i++)
    {
      cout << "Enumerate indices at index:" << i+1 << " = ";
      int cty = enumerate[i].size();
      for (int j=0;j<cty;j++)
        cout << enumerate[i][j] << ", ";
      cout << endl;
    }
/*
    for (intVectorIt ix=enumerate.begin();ix!=enumerate.end();++ix)
    {
      cout << "Enumerate: ";
      for (intVectorIt iy=ix.begin();iy!=ix.end();++iy)
      {
        cout << iy;
      }
      cout << endl;
    }
*/
  }

  void Enumerate::remove(unsigned int i)
  { //enumerate.remove(i);
  }

  void Enumerate::insert(intVector v)
  {
  enumerate.insert(enumerate.end(),v); }

  void Enumerate::parseAllIndices(vector<string> indices)
  {
    enumerate.clear();
    vector<string>::iterator it;
    for (it=indices.begin();it!=indices.end();++it)
      enumerate.insert(enumerate.end(),parse(*it));
  }
  bool Enumerate::test1()
  {
    Enumerate e;
    vector<string> s;
    s.insert(s.end(),"1,3");
    s.insert(s.end(),"10-11,12-13");
    e.parseAllIndices(s);
    e.start();
    assert(!e.done());
    assert(e.idx(0)==1);
    assert(e.idx(1)==10);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==1);
    assert(e.idx(1)==11);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==1);
    assert(e.idx(1)==12);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==1);
    assert(e.idx(1)==13);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==3);
    assert(e.idx(1)==10);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==3);
    assert(e.idx(1)==11);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==3);
    assert(e.idx(1)==12);
    e.next();
    assert(!e.done());
    assert(e.idx(0)==3);
    assert(e.idx(1)==13);
    e.next();
    assert(e.done());
    return true;
  }

  bool Enumerate::passedTest()
  {
    assert(Enumerate::test1());
    return true;
  }

