#pragma hdrstop
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include "equation.h"
#include "Assertions.h"
#include <iostream>
using namespace std;

/*
  Grammar for the expression evaluator below

  <expr>  ::=<term>+<expr> |
             <term>-<expr> |
             <term>
  <func>  ::= ln | exp | sin | cos | tan
  <str>   ::= [a-zA-Z]+
  <num>   ::= [0-9.][0-9e+-.]*
  <id>    ::= <str>{[<num>{,<num>,<num>,...}]}
  <term>  ::=<power>*<term> |
             <power>/<term> |
             <power>
  <power> ::=<factor>^<power> |
             <factor>
  <factor>::=(<expr>) |
             <id> |
             <num> |
             <func>(<expr>) |
             -<num>
*/

string idx;

typedef enum {Str,Equals,Plus,Minus,Mult,Divide,LPar,RPar,End,Err,
              Num,Comma,LBrac,RBrac,Ln,Exp,Sin,Cos,Tan,Caret} token;
typedef struct
  {
    token Last;
    char Text[80];
    int Pos;
    char *Line;
  } ScanInfo;

int SyntaxError;
char Line1[1000],Line2[1000],Line3[1000];
void ClearError()
  {
    SyntaxError=0;
  }
int Error(ScanInfo *i,char *s)
  {
    assert(s!=NULL);
    strncpy(Line1,i->Line,1000);
    Line1[999]='\0';
    if (i->Pos>0)
      sprintf(Line2,"%*s^",(i->Pos)-1," ");
    else
      strcpy(Line2,"^");
    strncpy(Line3,s,1000);
    Line3[999]='\0';
    SyntaxError=1;
    return 0;
  }

void Scan(ScanInfo *i)
  {
    assert(i!=NULL);
    int p;
    strcpy(i->Text,"");
    while(i->Line[i->Pos]<=' ' && i->Line[i->Pos]>'\0')
      i->Pos++;
    if (i->Line[i->Pos]=='=')
      i->Last=Equals;
    else if (i->Line[i->Pos]=='\0')
      i->Last=End;
    else if (i->Line[i->Pos]=='+')
      i->Last=Plus;
    else if (i->Line[i->Pos]=='-')
      i->Last=Minus;
    else if (i->Line[i->Pos]=='*')
      i->Last=Mult;
    else if (i->Line[i->Pos]=='/')
      i->Last=Divide;
    else if (i->Line[i->Pos]=='(')
      i->Last=LPar;
    else if (i->Line[i->Pos]==')')
      i->Last=RPar;
    else if (i->Line[i->Pos]=='[')
      i->Last=LBrac;
    else if (i->Line[i->Pos]==']')
      i->Last=RBrac;
    else if (i->Line[i->Pos]==',')
      i->Last=Comma;
    else if (i->Line[i->Pos]=='^')
      i->Last=Caret;
    else if (isalpha(i->Line[i->Pos]))
      {
        p=0;
//        while (isalpha(i->Line[i->Pos]))
        while (isalnum(i->Line[i->Pos]) || i->Line[i->Pos]=='_' ||
               i->Line[i->Pos]=='{' || i->Line[i->Pos]=='}' ||
               i->Line[i->Pos]==',')
          {
            i->Text[p]=i->Line[i->Pos];
            p++;
            i->Pos++;
          }
        i->Text[p]='\0';
        if (strcmpi(i->Text,"ln")==0) i->Last=Ln;
        else if (strcmpi(i->Text,"exp")==0) i->Last=Exp;
        else if (strcmpi(i->Text,"sin")==0) i->Last=Sin;
        else if (strcmpi(i->Text,"cos")==0) i->Last=Cos;
        else if (strcmpi(i->Text,"tan")==0) i->Last=Tan;
        else i->Last=Str;
        i->Pos--;
      }
    else if (isdigit(i->Line[i->Pos]) || i->Line[i->Pos]=='.')
      {
        p=0;
        while (isdigit(i->Line[i->Pos]) || i->Line[i->Pos]=='.' ||
               i->Line[i->Pos]=='e' ||i->Line[i->Pos]=='E' ||
               i ->Line[i->Pos]=='+' || i ->Line[i->Pos]=='-')
          {
            i->Text[p]=i->Line[i->Pos];
            p++;
            i->Pos++;
          }
        i->Text[p]='\0';
        i->Last=Num;
        i->Pos--;
      }
    else
      i->Last=Err;
    i->Pos++;
  }

void InitScan(ScanInfo *i,char *s)
  {
    assert(i!=NULL);
    assert(s!=NULL);
    i->Line=s;
    i->Pos=0;
    i->Last=Err;
    i->Text[0]='\0';
    Scan(i);
  }

float ID(ValueMap &data,ScanInfo *i)
  {
    assert(i!=NULL);
    float val;
    if (i->Last==Str)
      {
        if (data.find(string(i->Text))==data.end()) // Check for name alone
          {
            if (data.find(string(i->Text)+idx)==data.end()) // Check for name with {idx} post appended
            {
              Error(i,"Parameter missing.");
              Scan(i);
              return 0.0;
            }
            else
             val=data[string(i->Text)+idx];
             Scan(i);
             return val;
          }
        else
          {
            val=data[string(i->Text)];
            Scan(i);
            return val;
          }
      }
    Error(i,"Id missing.");
    Scan(i);
    return 0.0;
  }

float expr(ValueMap &data,ScanInfo *i);

float factor(ValueMap &data,ScanInfo *i)
  {
    assert(i!=NULL);
    token function;
    float temp;
    if (i->Last==LPar)
      {
        Scan(i);
        temp=expr(data,i);
        if (i->Last!=RPar)
          Error(i,") Missing.");
        else
          Scan(i);
        return temp;
      }
    else if (i->Last==Num)
      {
        temp=atof(i->Text);
        Scan(i);
        return temp;
      }
    else if (i->Last==Str)
      {
        return ID(data,i);
      }
    else if (i->Last==Ln || i->Last==Exp || i->Last==Sin || i->Last==Cos ||
             i->Last==Tan)
      {
        function=i->Last;
        Scan(i);
        if (i->Last!=LPar)
          {
            Error(i,"( Missing.");
            return 0.0;
          }
        else
          {
            Scan(i);
            temp=expr(data,i);
            if (i->Last!=RPar)
              Error(i,") Missing.");
            else
              Scan(i);
            if (function==Ln)
              {
                if (temp >0.0)
                  return log(temp);
                else
                  {
                    Error(i,"ln(n) where n<=0.0 error.  0.0 assumed");
                    return 0.0;
                  }
              }
            else if (function==Exp) return exp(temp);
            else if (function==Sin) return sin(temp);
            else if (function==Cos) return cos(temp);
            else if (function==Tan) return tan(temp);
            Error(i,"Unrecognized function. 0.0 assumed");
            return 0.0;
          }
      }
    else if(i->Last==Minus)
      {
        Scan(i);
        return -factor(data,i);
      }
    else
      {
        Error(i,"(,Number, or Parameter name expected. 0.0 assumed.");
        return 0.0;
      }
  }

float power(ValueMap &data,ScanInfo *i)
  {
    assert(i!=NULL);
    float a;
    a=factor(data,i);
    while (i->Last==Caret)
      {
        Scan(i);
        a=pow(a,factor(data,i));
      }
    return a;
  }

float term(ValueMap &data,ScanInfo *i)
  {
    assert(i!=NULL);
    float a,b;
    a=power(data,i);
    while (i->Last==Mult || i->Last==Divide)
      if (i->Last==Mult)
        {
          Scan(i);
          a*=power(data,i);
        }
      else if (i->Last==Divide)
        {
          Scan(i);
          b=power(data,i);
          if (b==0.0)
            {
              Error(i,"Divide by zero. Zero Assumed");
              a=0.0;
            }
          else
            a/=b;
        }
    return a;
  }

float expr(ValueMap &data,ScanInfo *i)
  {
    assert(i!=NULL);
    float a;
    a=term(data,i);
    while (i->Last==Plus || i->Last==Minus)
      if (i->Last==Plus)
        {
          Scan(i);
          a+=term(data,i);
        }
      else if (i->Last==Minus)
        {
          Scan(i);
          a-=term(data,i);
        }
    return a;
  }

bool eval(string alias,string validx,string equation,ValueMap &data,char **l1,char **l2,char **l3)
{
  assert(l1!=NULL);
  assert(l2!=NULL);
  assert(l3!=NULL);
  ScanInfo S;
  InitScan(&S,(char *)equation.c_str());
  idx=validx;
  data[alias]=expr(data,&S);
  if (SyntaxError)
    {
      *l1=Line1;
      *l2=Line2;
      *l3=Line3;
      return false;
    }
  else
    return true;
}

bool evalTest1()
{
  char *line1,*line2,*line3;
  ValueMap d;
  d["a"]=10.0;
  d["b{1,2}"]=20.0;
  assert(eval("a","","(a * b{1,2} + 20 + -10)/10)",d,&line1,&line2,&line3));
  assert(d["a"]==21);
  return true;
}

bool evalTest2()
{
  char *line1,*line2,*line3;
  ValueMap d;
  d["a"]=3.14159/2.0;
  d["b"]=1.5;
  d["c"]=1.0;
  assert(eval("c","","tan(b) - sin(b) / cos(b)",d,&line1,&line2,&line3));
  assert(d["c"]==0.0);
  assert(eval("a","","sin(a) ^ 2 + cos(a) ^ 2",d,&line1,&line2,&line3));
  assert(d["a"]==1.0);
  assert(eval("b","","sin(b) ^ 2 + cos(b) ^ 2",d,&line1,&line2,&line3));
  assert(d["b"]==1.0);
  return true;
}

bool evalTest3()
{
  char *line1,*line2,*line3;
  ValueMap d;
  d["a"]=3.14159/2.0;
  d["b"]=0.0;
  d["c"]=0.0;
  assert(eval("c","","exp(ln(a))",d,&line1,&line2,&line3));
  assert(d["c"]==d["a"]);
  return true;
}

bool evalPassedTest()
{
  assert(evalTest1());
  assert(evalTest2());
  assert(evalTest3());
  return true;
}

