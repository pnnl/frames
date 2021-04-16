#include "menusmal.h"
//#include <windows.h>
#include <process.h>
#include <stdio.h>
#include <conio.h>
#include <string.h>
#include "memhand.h"
#include "newinfo.h"

//FILE *err;

typedef struct
  {
	 int sx,sy,nc,nl;
	 char **data;
  }simplewindow;

void ExitApplication(int status)
  {
	 clrscr();
//	 fclose(err);
	 exit(status);
  }

void InitApplication(void)
  {
//	 err=fopen("inputgen.err","at+");
	 clrscr();
  }

void MemoryHandler(void)
  {
    static char *memmes[3]=
      {
        "Out of Memory",
        "In MENUCURS",
        "Press any key to exit"
      };
    Message(3,memmes);
    ExitApplication(1);
  }

void NullHandler(void)
  {
    static char *memmes[3]=
      {
		  "NULL Memory pointer",
		  "In MENUCURS",
        "Press any key to exit"
      };
    Message(3,memmes);
    ExitApplication(1);
  }

static void clear(simplewindow *win)
  {
     int i;
     int j;
     for (i=0;i<win->nl;i++)
       {
         for (j=0;j<win->nc;j++)
			  {
             win->data[i][j]=' ';
           }
			win->data[i][j]='\0';
		 }
  }

static void solid(simplewindow *win,int fill)
  {
     int i;
     int j;
     for (i=0;i<win->nl;i++)
       {
         for (j=0;j<win->nc;j++)
           {
             win->data[i][j]=fill;
           }
         win->data[i][j]='\0';
		 }
  }

static simplewindow *newwin(int nl,int nc,int sy,int sx)
  {
     int i;
     simplewindow *win;

     win=(simplewindow *)getmem(sizeof(simplewindow));
     win->sx=sx;
     win->sy=sy;
     win->nc=nc;
     win->nl=nl;
     win->data=(char **)getmem(sizeof(char *)*nl);
     for (i=0;i<nl;i++)
       win->data[i]=(char *)getmem(sizeof(char)*(nc+1));
     solid(win,' ');
     return win;
  }

static void box(simplewindow *win,char vtype,char htype)
  {
	 int i;
    if (vtype==0)
      vtype =179;
    else
      vtype ='+';
    if (htype==0)
      htype =196;
    else
      htype ='+';

    for (i=0;i<win->nc;i++)
      {
        win->data[0][i]=htype;
        win->data[win->nl-1][i]=htype;
		}
    for (i=1;i<win->nl-1;i++)
      {
		  win->data[i][0]=vtype;
		  win->data[i][win->nc-1]=vtype;
      }
    win->data[win->nl-1][win->nc-1]=217;
    win->data[win->nl-1][0]=192;
    win->data[0][win->nc-1]=191;
    win->data[0][0]=218;
  }

static void mvprintw(simplewindow *win,int y,int x,char *string)
  {
    int len1,len2;
    len1=win->nc-x;
    len2=strlen(string);
    if (len2>len1)
		strncpy(&win->data[y][x],string,len1);
    else
      strncpy(&win->data[y][x],string,len2);
  }

static void refresh(simplewindow *win)
  {
    int i;
    for (i=0;i<win->nl;i++)
      {
        gotoxy(win->sx,win->sy+i);
        printf(win->data[i]);
      }
  }

static void delwin(simplewindow *win)
  {
    int i;
	 for (i=0;i<win->nl;i++)
      putmem(win->data[i]);
    putmem(win->data);
  }

static simplewindow *messagewindow;

int TempMessage(int NumItems,char **text)
  {
    int l=0,i,t;
    for (i=0;i<NumItems;i++)
      {
      	t=strlen(text[i]);
	      if (t>l) l=t;
      }
    messagewindow=newwin(NumItems+2,l+4,(24-NumItems-2)/2,(80-l-2)/2);
    box(messagewindow,0,0);
    for (i=0;i<NumItems;i++)
		{
      	t=(l+4-strlen(text[i]))/2;
	      mvprintw(messagewindow,1+i,t,text[i]);
       fprintf(mlog,"%s\n",text[i]);
		}
	 refresh(messagewindow);
    return 0;
  }

void ClearTempMessage(int Handle)
  {
    clear(messagewindow);
    refresh(messagewindow);
    delwin(messagewindow);
  }

void Message(int NumItems,char **text)
  {
    int i/*BRW ,k*/;
	 i=TempMessage(NumItems,text);
	/*BRW k=*/getch();
	 ClearTempMessage(i);
  }
