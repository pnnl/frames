#pragma hdrstop
#include "equation.h"
#include <math.h>

/*
  Grammar for the expression evaluator below

  <expr>  :: = <term>+<expr> |
             <term>-<expr> |
             <term>
  <func>  :: = ln | exp | sin | cos | tan
  <str>   :: = [a-zA-Z]+
  <num>   :: = [0-9.][0-9e+-.]*
  <id>    :: = <str>{[<num>{,<num>,<num>,...}]}
  <term>  :: = <power>*<term> |
             <power>/<term> |
             <power>
  <power> :: = <factor>^<power> |
             <factor>
  <factor>:: = (<expr>) |
             <id> |
             <num> |
             <func>(<expr>) |
             -<num>
*/

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
char Line1[LARGESTRING];
char Line2[LARGESTRING];
char Line3[LARGESTRING];

void ClearError()
  {
    SyntaxError = 0;
  }
int Error(ScanInfo *i,char *s)
  {
    strncpy(Line1,i->Line,LARGESTRING);
    Line1[LARGESTRING-1] = '\0';
    if (i->Pos>0)
      sprintf(Line2,"%*s^",(i->Pos)-1," ");
    else
      rstrcpy(Line2,"^");
    strncpy(Line3,s,LARGESTRING);
    Line3[LARGESTRING-1] = '\0';
    SyntaxError = 1;
    return 0;
  }

void Scan(ScanInfo *i)
  {
    int p;
    rstrcpy(i->Text,"");
    while(i->Line[i->Pos]<= ' ' && i->Line[i->Pos]>'\0')
      i->Pos++;
    if (i->Line[i->Pos] == '=')
      i->Last = Equals;
    else if (i->Line[i->Pos] == '\0')
      i->Last = End;
    else if (i->Line[i->Pos] == '+')
      i->Last = Plus;
    else if (i->Line[i->Pos] == '-')
      i->Last = Minus;
    else if (i->Line[i->Pos] == '*')
      i->Last = Mult;
    else if (i->Line[i->Pos] == '/')
      i->Last = Divide;
    else if (i->Line[i->Pos] == '(')
      i->Last = LPar;
    else if (i->Line[i->Pos] == ')')
      i->Last = RPar;
    else if (i->Line[i->Pos] == '[')
      i->Last = LBrac;
    else if (i->Line[i->Pos] == ']')
      i->Last = RBrac;
    else if (i->Line[i->Pos] == ',')
      i->Last = Comma;
    else if (i->Line[i->Pos] == '^')
      i->Last = Caret;
    else if (isalpha(i->Line[i->Pos]))
      {
        p = 0;
//        while (isalpha(i->Line[i->Pos]))
        while (isalnum(i->Line[i->Pos]))
          {
            i->Text[p] = i->Line[i->Pos];
            p++;
            i->Pos++;
          }
        i->Text[p] = '\0';
        if (!rstrcmpi(i->Text,"ln")) i->Last = Ln;
        else if (!rstrcmpi(i->Text,"exp")) i->Last = Exp;
        else if (!rstrcmpi(i->Text,"sin")) i->Last = Sin;
        else if (!rstrcmpi(i->Text,"cos")) i->Last = Cos;
        else if (!rstrcmpi(i->Text,"tan")) i->Last = Tan;
        else i->Last = Str;
        i->Pos--;
      }
    else if (isdigit(i->Line[i->Pos]) || i->Line[i->Pos] == '.')
      {
        p = 0;
        while (isdigit(i->Line[i->Pos]) || i->Line[i->Pos] == '.' ||
               i->Line[i->Pos]  == 'e'    || i->Line[i->Pos] == 'E' ||
               i ->Line[i->Pos]  == '+'   || i ->Line[i->Pos] == '-')
          {
            i->Text[p] = i->Line[i->Pos];
            p++;
            i->Pos++;
          }
        i->Text[p] = '\0';
        i->Last = Num;
        i->Pos--;
      }
    else
      i->Last = Err;
    i->Pos++;
  }

void InitScan(ScanInfo *i,char *s)
  {
    i->Line = s;
    i->Pos = 0;
    i->Last = Err;
    i->Text[0] = '\0';
    Scan(i);
  }

double ID(SUMData *data,ScanInfo *i)
  {
    int j;
    if (i->Last == Str)
      {
        j = 0;
        while (j < data->VarCount && rstrcmpi(i->Text,data->Vars[j].Alias))
          j++;
        if (j == data->VarCount)
          {
            Error(i,"Parameter missing.");
            Scan(i);
            return 0.0;
          }
        else
          {
            Scan(i);
            return atof(data->Vars[j].p.valu);
          }
      }
    Error(i,"Id missing.");
    Scan(i);
    return 0.0;
  }

double expr(SUMData *pf,ScanInfo *i);

double factor(SUMData *gf,ScanInfo *i)
  {
    token function;
    double temp;
    if (i->Last == LPar)
      {
        Scan(i);
        temp = expr(gf,i);
        if (i->Last != RPar)
          Error(i,") Missing.");
        else
          Scan(i);
        return temp;
      }
    else if (i->Last == Num)
      {
        temp = atof(i->Text);
        Scan(i);
        return temp;
      }
    else if (i->Last == Str)
      {
        return ID(gf,i);
      }
    else if (i->Last == Ln || i->Last == Exp || i->Last == Sin || i->Last == Cos ||
             i->Last == Tan)
      {
        function = i->Last;
        Scan(i);
        if (i->Last != LPar)
          {
            Error(i,"( Missing.");
            return 0.0;
          }
        else
          {
            Scan(i);
            temp = expr(gf,i);
            if (i->Last != RPar)
              Error(i,") Missing.");
            else
              Scan(i);
            if (function == Ln)
              {
                if (temp > 0.0)
                  return log(temp);
                else
                  {
                    Error(i,"ln(n) where n<= 0.0 error.  0.0 assumed");
                    return 0.0;
                  }
              }
            else if (function == Exp) return exp(temp);
            else if (function == Sin) return sin(temp);
            else if (function == Cos) return cos(temp);
            else if (function == Tan) return tan(temp);
            Error(i,"Unrecognized function. 0.0 assumed");
            return 0.0;
          }
      }
    else if (i->Last == Minus)
      {
        Scan(i);
        return -factor(gf,i);
      }
    else
      {
        Error(i,"(,Number, or Parameter name expected. 0.0 assumed.");
        return 0.0;
      }
  }

double power(SUMData *gf,ScanInfo *i)
  {
    double a;
    a = factor(gf,i);
    while (i->Last == Caret)
      {
        Scan(i);
        a = pow(a,factor(gf,i));
      }
    return a;
  }

double term(SUMData *gf,ScanInfo *i)
  {
    double a,b;
    a = power(gf,i);
    while (i->Last == Mult || i->Last == Divide)
      if (i->Last == Mult)
        {
          Scan(i);
          a *= power(gf,i);
        }
      else if (i->Last == Divide)
        {
          Scan(i);
          b = power(gf,i);
          if (b == 0.0)
            {
              Error(i,"Divide by zero. Zero Assumed");
              a = 0.0;
            }
          else
            a /= b;
        }
    return a;
  }

double expr(SUMData *gf,ScanInfo *i)
  {
    double a;
    a = term(gf,i);
    while (i->Last == Plus || i->Last == Minus)
      if (i->Last == Plus)
        {
          Scan(i);
          a += term(gf,i);
        }
      else if (i->Last == Minus)
        {
          Scan(i);
          a -= term(gf,i);
        }
    return a;
  }

int Equation(SUMData *data,char **l1,char **l2,char **l3)
  {
    int i;
    double f;
    ScanInfo S;
    ClearError();
    for (i = 0; i<data->VarCount && !SyntaxError; i++)
      {
        if (data->Vars[i].HasEquation())
          {
            InitScan(&S,data->Vars[i].Equation);
            f = expr(data,&S);
            sprintf(data->Vars[i].p.valu,"%g",f);
          }
      }
    if (SyntaxError)
      {
        *l1 = Line1;
        *l2 = Line2;
        *l3 = Line3;
        return 0;
      }
    else
      return 1;
  }
