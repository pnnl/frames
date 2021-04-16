#ifndef TEST_H
#define TEST_H

#pragma warning(disable:4786)

#include <vector>
#include <iostream>
#include <fstream>
#include <strstream>
#include <string>
#include <map>
#include <CRTDBG.H>
#include <stdio.h>

using namespace std;

extern ofstream output,doc;

//#ifdef _DEBUG
extern void main(int argc, char* argv[]);
extern void PerformTests(); // The test suite should implement these two functions
extern void CleanUp();      // it should make multiple calls to macro addTest
//#endif
//-------------------------------------------------------------------------------------------
class LineInfo
{
	static int count;
private:
	string file,variable;
	int line;
	bool notTracked;
	int when;
public://-------------------------------------------------------------------------------------------
	LineInfo(int newLine=0,const char *newFile="Error",const char* newVariable="Error",bool newNotTracked=false)
	{
		variable=newVariable;
		line=newLine;
		file=string(newFile);
		notTracked=newNotTracked;
		when=++count;
	}
	//-------------------------------------------------------------------------------------------
	string Describe()
	{
		strstream s;
		if (notTracked) 
			s << "Delete untracked " ;
		else
			s << "Not deleted ";
		s << when << "-" << variable << " at " << file << "(" << line << ")" << '\0';
		return string(s.str());
	}
};
//-------------------------------------------------------------------------------------------
class Test // The fundamental concept of a test routine.
{
    public:
	string Name,Description;
	void Describe(ofstream &out)
	{
		out << "<tr><td valign='top'><a name='" << Name << "'>" << Name << "</td><td valign='top'>" << Description <<"</td><td valign='top'>";
	}//-------------------------------------------------------------------------------------------
	void DescribeStatus(ofstream &out)
	{
 	    out << "<tr><td valign='top'>" << Name.c_str() << "</td>";
	}//-------------------------------------------------------------------------------------------
	virtual bool doTest(ofstream &status,ofstream &doc)
	{
		return true;
	}//-------------------------------------------------------------------------------------------
	bool Error(ofstream &status,const char *msg,int LineNum,const char *File)
	{
		ErrorPreamble(status,msg);
		return ErrorPostamble(status,LineNum,File);
	}//-------------------------------------------------------------------------------------------
	void ErrorPreamble(ofstream &status,const char *msg)
	{
		status << "<td bgcolor=pink><img src='/Frames-V2/bugs.gif'><br><pre>" << msg ;		
	}//-------------------------------------------------------------------------------------------
	bool ErrorPostamble(ofstream &status,int LineNum,const char *File)
	{
		status << "<br>"<< File <<"[" << LineNum << "]<br>" << "</td></tr>" << endl;	
		return false;
	}
};
//-------------------------------------------------------------------------------------------
class Tests 
{
  ofstream status,doc;
  map<void *,LineInfo> mapOfAllocs;
  vector<Test*> tests;
public://-------------------------------------------------------------------------------------------
	void OutputLeaks(ofstream &status)
	{
		map<void *,LineInfo>::iterator it;
		if (mapOfAllocs.size()!=0)
		{
		  status << "Summary of allocations that were not deallocatted" <<  endl;
		  for (it=mapOfAllocs.begin();it!=mapOfAllocs.end();++it)
			status << (int)it->first << it->second.Describe() << endl;
		  status << "End of Summary" << endl;
		}
		else status << "No leaks tracked" << endl;
	}//-------------------------------------------------------------------------------------------
	void New(void *arg,int line,const char *file,const char *var)
	{
		mapOfAllocs[arg]=LineInfo(line,file,var); 
	}//-------------------------------------------------------------------------------------------
	void Delete(void *arg,int line,const char *file,const char *var)
	{
      if (mapOfAllocs.find(arg)==mapOfAllocs.end()) 
        mapOfAllocs[arg]=LineInfo(line,file,var,true); 
      else 
        mapOfAllocs.erase(arg); 
	}//-------------------------------------------------------------------------------------------
	bool Assert(Test *t,bool con,int line,const char *file,const char *strcond)
	{
  	  doc <<"<b>" << strcond << "</b><br>";  
	  if (!con) 
	  {
		  doc << "<img src='/Frames-V2/bugs.gif'><br>" << endl;
		  t->Error(status,strcond,line,file);
	  }
	  doc << " <i>" << file << "(" << line << ")</i><br> " << endl;
	  return con;
	}//-------------------------------------------------------------------------------------------
	void AddTest(Test * t) 
	{ 
		tests.insert(tests.end(),t); 
	}//-------------------------------------------------------------------------------------------
	virtual void PerformTests()
	{
	}//-------------------------------------------------------------------------------------------
	void doTests()
	{
	  status.open("status.html");
	  doc.open("docs.html");
	  try
	  {
	  status << "<html><head></head><body>" << endl;
	  status << "Details of the test can be found in <a href='docs.html'>docs.html</a>" << endl;
	  doc << "<html><head></head><body>" <<endl;
	  doc << "Overall status be found in <a href='status.html'>status.html</a>" << endl;
	  status << "<table border=1 width='100%'>" << endl;
	  status << "<tr><th width='50%'>Name</th><th width='50%'>Status</th></tr>" << endl;
	  doc << "<table border=1 width='100%'>" << endl;
	  doc << "<tr><th width='10%'>Name</th><th width='20%'>Description</th><th width='70%'>Steps</th></tr>";
	  for (int j=0;j<1; j++)
		for (int i=0;i<tests.size();i++)
		{
		  tests[i]->Describe(doc);
		  tests[i]->DescribeStatus(status);
		  if (tests[i]->doTest(status,doc))
			status << "<td bgcolor='lightgreen'>Passed</td></tr>" << endl;
		  doc << "</td></tr>" <<endl;
		}
	  }
	  catch (char *str)
	  {
			status << "<tr><td valign='top'>char * Exception</td><td bgcolor='pink'>Failed<br>";
			status << str ;
			status << "</td></tr>" << endl;	
			doc << "</td></tr>" << endl;
	  }
	  catch (int err)
	  {
			status << "<tr><td valign='top'>int Exception</td><td bgcolor='pink'>Failed<br>";
			status << err ;
			status << "</td></tr>" << endl;	
			doc << "</td></tr>" << endl;
	  }
	//  catch (...)
	//  {
	//	  status << "<tr><td valign='top'>Unknown Exception</td><td bgcolor='pink'>Failed<br>";
	//	  status << "</td></tr>" << endl;	
	//      doc << "</td></tr>" << endl;
	//  }
	  status << "</table></body></html>" << endl;
	  doc << "</table></body></html>" << endl;
	  status.close();
	  doc.close();	
	}
	~Tests()
	{
		for (int i=0;i<tests.size();i++)
			delete tests[i];
	}
};
//-------------------------------------------------------------------------------------------
#ifdef _DEBUG
  #define TrackLocation(arg) tests.New((void *)arg,__LINE__,__FILE__,#arg)
  #define TrackDelete(arg)   { tests.Delete  ((void *)arg,__LINE__,__FILE__,#arg); delete arg; arg=NULL; }
#else
  #define TrackLocation(arg) 
  #define TrackDelete(arg)   delete arg; arg=NULL
#endif
  
#define assert(cond) \
{ \
	if (!tests.Assert(this,(cond),__LINE__,__FILE__,#cond)) \
	{ \
		return false; \
    } \
}

extern Tests tests;
extern _CrtMemState b;
extern strstream debugMessages;

int MyReportHook( int reportType, char *message, int *returnValue );
//-------------------------------------------------------------------------------------------
class GrabHeapState:public Test {
public:
	GrabHeapState() 
	{
		Description="<p>A test that captures the state of the heap.  This is checked later with"
			"<a href='#CheckHeapForLeaks'>CheckHeapForLeaks</a> by checking 1)that any allocation"
			"of memory is also deallocated and 2) that Windows sees no other leaks have"
			"occurred.";
			Name="GrabHeapState";
	}
	virtual bool doTest(ofstream &status,ofstream &doc) {
		doc << "Heap Captured" << endl;
       _CrtMemCheckpoint( &b );
		return true;
	}
};
//-------------------------------------------------------------------------------------------
class CheckHeapForLeaks:public Test {
public:
	CheckHeapForLeaks() 
	{
		Description="<p>A test that checks the state of the heap against an earlier copy."
			"The earlier copy is captured with <a href='#GrabHeapState'>GrabHeapState</a>."
			"These checks are done by 1)that any allocation"
			"of memory is also deallocated and 2) that Windows believes no other leaks have"
			"occurred.";
		Name="CheckHeapForLeaks";
	}
	virtual bool doTest(ofstream &status,ofstream &doc) 
	{
        _CrtMemState e;
		doc << "Comparison of heaps and checking for leaks";
	   _CrtSetReportHook(MyReportHook );
	   _CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_DEBUG );
	   _CrtSetReportMode( _CRT_ERROR, _CRTDBG_MODE_DEBUG );
	   _CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_DEBUG );
		_CrtMemState diff;
       _CrtMemCheckpoint( &e );
	   if ( _CrtMemDifference( &diff, &b, &e ) )
	   {
		 _CrtMemDumpStatistics(&diff);
		 _CrtDumpMemoryLeaks();
		 debugMessages << '\0';
		 ErrorPreamble(status,"Memory Leak<br><pre>");
		 tests.OutputLeaks(status);
		 status << debugMessages.str() << "<br></pre>";
		 return ErrorPostamble(status,__LINE__,__FILE__);
	   }
	   return true;
	}
};
//-------------------------------------------------------------------------------------------
#endif