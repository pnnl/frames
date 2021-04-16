#ifndef MSCPP5_H
#define MSCPP5_H
//#include <windows.h>

#define PASS _declspec(dllimport) __cdecl 

///////////////////////////////////////////////////////////////////////////////
//  Code:    HWIR_IO.h
//  Date:    3/2/98
//  Programmer:  Mitch Pelton,Karl Castleton and Bonnnie Hooopes
//  Modified
//
///////////////////////////////////////////////////////////////////////////////
typedef void MyExit();
#ifdef __cplusplus
  extern "C" {
#endif
 void  PASS _SetArgs(char *gname,MyExit *callback,int LeaveWarn);
 int  PASS _IOOk(void);
 int  PASS  _NumArgs();
 void  PASS _AddGroup(char *gname);
 void  PASS _AddRWGroup(char *gname);
 void PASS  _RemoveGroup(char *gname);
 int  PASS  _GetArgInt(int ArgIndex);
 void  PASS  _GetArgString(int ArgIndex, char *Argument);
 void  PASS  _OpenGroups();
 void  PASS  _CloseGroups(void);
 void  PASS  _Error(char *Description);
 void  PASS  _Warning(char *Description);

///////////////////////////////////////////////////////////////////////////////
//   routines for reading integer variables
///////////////////////////////////////////////////////////////////////////////
 int  PASS  _ReadInt(char *_group,char *_var,char *_unit);
 int  PASS  _ReadInt1(char *_group,char *_var,char *_unit, int _1);
 int  PASS  _ReadInt2(char *_group,char *_var,char *_unit, int _1,int _2);
 int  PASS  _ReadInt3(char *_group,char *_var,char *_unit, int _1,int _2,int _3);
 int  PASS  _ReadInt4(char *_group,char *_var,char *_unit, int _1,int _2,int _3,int _4);
 int  PASS  _ReadInt5(char *_group,char *_var,char *_unit, int _1,int _2,int _3,int _4,int _5);
 int  PASS  _ReadInt6(char *_group,char *_var,char *_unit, int _1,int _2,int _3,int _4,int _5,int _6);

///////////////////////////////////////////////////////////////////////////////
//   routines for reading real(double); variables
///////////////////////////////////////////////////////////////////////////////
 double  PASS  _ReadReal(char *_group,char *_var,char *_unit);
 double  PASS  _ReadReal1(char *_group,char *_var,char *_unit,int _1);
 double  PASS  _ReadReal2(char *_group,char *_var,char *_unit,int _1,int _2);
 double  PASS  _ReadReal3(char *_group,char *_var,char *_unit,int _1,int _2,int _3);
 double  PASS  _ReadReal4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4);
 double  PASS  _ReadReal5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5);
 double  PASS  _ReadReal6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6);

///////////////////////////////////////////////////////////////////////////////
//   routines for reading logical variables
///////////////////////////////////////////////////////////////////////////////
 int  PASS  _ReadLog(char *_group,char *_var,char *_unit);
 int  PASS  _ReadLog1(char *_group,char *_var,char *_unit,int _1);
 int  PASS  _ReadLog2(char *_group,char *_var,char *_unit,int _1,int _2);
 int  PASS  _ReadLog3(char *_group,char *_var,char *_unit,int _1,int _2,int _3);
 int  PASS  _ReadLog4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4);
 int  PASS  _ReadLog5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5);
 int  PASS  _ReadLog6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6);

///////////////////////////////////////////////////////////////////////////////
//   routines for reading string variables
///////////////////////////////////////////////////////////////////////////////
 void  PASS  _ReadString(char *_group,char *_var,char *_unit,char *_str);
 void  PASS  _ReadString1(char *_group,char *_var,char *_unit,int _1,char *_str);
 void  PASS  _ReadString2(char *_group,char *_var,char *_unit,int _1,int _2,char *_str);
 void  PASS  _ReadString3(char *_group,char *_var,char *_unit,int _1,int _2,int _3,char *_str);
 void  PASS  _ReadString4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,char *_str);
 void  PASS  _ReadString5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,char *_str);
 void  PASS  _ReadString6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6,char *_str);

///////////////////////////////////////////////////////////////////////////////
//  Export routines for writing integer variables
///////////////////////////////////////////////////////////////////////////////
 void  PASS  _WriteInt(char *_group,char *_var,char *_unit,int _val);
 void  PASS  _WriteInt1(char *_group,char *_var,char *_unit,int _1,int _val);
 void  PASS  _WriteInt2(char *_group,char *_var,char *_unit,int _1,int _2,int _val);
 void  PASS  _WriteInt3(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _val);
 void  PASS  _WriteInt4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _val);
 void  PASS  _WriteInt5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _val);
 void  PASS  _WriteInt6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6,int _val);

///////////////////////////////////////////////////////////////////////////////
//  Export routines for writing real(double); variables
///////////////////////////////////////////////////////////////////////////////
 void  PASS  _WriteReal(char *_group,char *_var,char *_unit,double _val);
 void  PASS  _WriteReal1(char *_group,char *_var,char *_unit,int _1,double _val);
 void  PASS  _WriteReal2(char *_group,char *_var,char *_unit,int _1,int _2,double _val);
 void  PASS  _WriteReal3(char *_group,char *_var,char *_unit,int _1,int _2,int _3,double _val);
 void  PASS  _WriteReal4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,double _val);
 void  PASS  _WriteReal5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,double _val);
 void  PASS  _WriteReal6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6,double _val);
///////////////////////////////////////////////////////////////////////////////
//  Export routines for writing logical variables
///////////////////////////////////////////////////////////////////////////////
 void  PASS  _WriteLog(char *_group,char *_var,char *_unit,int _val);
 void  PASS  _WriteLog1(char *_group,char *_var,char *_unit,int _1,int _val);
 void  PASS  _WriteLog2(char *_group,char *_var,char *_unit,int _1,int _2,int _val);
 void  PASS  _WriteLog3(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _val);
 void  PASS  _WriteLog4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _val);
 void  PASS  _WriteLog5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _val);
 void  PASS  _WriteLog6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6,int _val);

///////////////////////////////////////////////////////////////////////////////
//  Export routines for writing string variables
///////////////////////////////////////////////////////////////////////////////
 void  PASS  _WriteString(char *_group,char *_var,char *_unit,char *_val);
 void  PASS  _WriteString1(char *_group,char *_var,char *_unit,int _1,char *_val);
 void  PASS  _WriteString2(char *_group,char *_var,char *_unit,int _1,int _2,char *_val);
 void  PASS  _WriteString3(char *_group,char *_var,char *_unit,int _1,int _2,int _3,char *_val);
 void  PASS  _WriteString4(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,char *_val);
 void  PASS  _WriteString5(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,char *_val);
 void  PASS  _WriteString6(char *_group,char *_var,char *_unit,int _1,int _2,int _3,int _4,int _5,int _6,char *_val);
#ifdef __cplusplus
}
#endif

 #define IOOk _IOOk
 #define NumArgs _NumArgs
 #define AddGroup _AddGroup
 #define AddRWGroup _AddRWGroup
 #define RemoveGroup _RemoveGroup
 #define SetArgs _SetArgs
 #define GetArgInt _GetArgInt
 #define GetArgString _GetArgString
 #define OpenGroups _OpenGroups
 #define CloseGroups _CloseGroups
 #define Error _Error
 #define Warning _Warning
 #define ReadInt _ReadInt
 #define ReadInt1 _ReadInt1
 #define ReadInt2 _ReadInt2
 #define ReadInt3 _ReadInt3
 #define ReadInt4 _ReadInt4
 #define ReadInt5 _ReadInt5
 #define ReadInt6 _ReadInt6
 #define ReadReal _ReadReal
 #define ReadReal1 _ReadReal1
 #define ReadReal2 _ReadReal2
 #define ReadReal3 _ReadReal3
 #define ReadReal4 _ReadReal4
 #define ReadReal5 _ReadReal5
 #define ReadReal6 _ReadReal6
 #define ReadLog _ReadLog
 #define ReadLog1 _ReadLog1
 #define ReadLog2 _ReadLog2
 #define ReadLog3 _ReadLog3
 #define ReadLog4 _ReadLog4
 #define ReadLog5 _ReadLog5
 #define ReadLog6 _ReadLog6
 #define ReadString _ReadString
 #define ReadString1 _ReadString1
 #define ReadString2 _ReadString2
 #define ReadString3 _ReadString3
 #define ReadString4 _ReadString4
 #define ReadString5 _ReadString5
 #define ReadString6 _ReadString6
 #define WriteInt _WriteInt
 #define WriteInt1 _WriteInt1
 #define WriteInt2 _WriteInt2
 #define WriteInt3 _WriteInt3
 #define WriteInt4 _WriteInt4
 #define WriteInt5 _WriteInt5
 #define WriteInt6 _WriteInt6
 #define WriteReal _WriteReal
 #define WriteReal1 _WriteReal1
 #define WriteReal2 _WriteReal2
 #define WriteReal3 _WriteReal3
 #define WriteReal4 _WriteReal4
 #define WriteReal5 _WriteReal5
 #define WriteReal6 _WriteReal6
 #define WriteLog _WriteLog
 #define WriteLog1 _WriteLog1
 #define WriteLog2 _WriteLog2
 #define WriteLog3 _WriteLog3
 #define WriteLog4 _WriteLog4
 #define WriteLog5 _WriteLog5
 #define WriteLog6 _WriteLog6
 #define WriteString _WriteString
 #define WriteString1 _WriteString1
 #define WriteString2 _WriteString2
 #define WriteString3 _WriteString3
 #define WriteString4 _WriteString4
 #define WriteString5 _WriteString5
 #define WriteString6 _WriteString6

#endif

