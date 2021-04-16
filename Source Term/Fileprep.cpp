#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<math.h>
#include<new.h>

#include"strm1.h"
#include"fcsv.h"
#include"fileprep.h"
#include"misc3.h"
#include"gid.h"

int shift=0;
char buf[MAXPATH];

void fileprep(int argc, char *argv[])
{
  if( argc == 6 )
    {
    strcpy(site.gid, argv[1]);
    strcpy(site.run, argv[2]);
    site.idx=atoi(argv[3]);
    strcpy(site.name, argv[5]);

    strcpy(buf, site.run);
    strcat(buf, ".err");
    if((err.open(buf,WRITE))== 0)
      {
      fprintf(stderr,"Cannot open error file %s\n", buf);
      go_die(3, "dumm");
      }
    else
      {
      err.write(dateoutst());
      err.write(timeoutst());
      err.writeln();
      }

    strcpy(buf, site.gid);
    strcat(buf, ".gid");
    if ( (data.g = Open_GID(buf))==NULL )
      {
      fprintf(stderr,"Cannot open Global Input Data file %s\n", buf);
      err.write("Cannot open Global Input Data file");
      err.write(buf);
      go_die(3,"dumm");
      }
    if (!Load_GID(data.g,site.idx,site.name))
      {
      fprintf(stderr,"Bad GID File %s\n",buf);
      err.write("Bad GID File");
      err.write(buf);
      go_die(3,"dumm");
      }

    strcpy(buf, site.run);
    strcat(buf, ".sdl");
    if((sdl.open(buf,WRITE))== 0)
      {
      fprintf(stderr,"Cannot open data work file %s\n", buf);
      err.write("Cannot open data work file");
      err.write(buf);
      go_die(3,buf);
      }

    sls = new SLS(err, site.run);

    }
  else
    {
    fprintf(stderr,"Usage:  strm1 <FUIName> <RunName> <Site#> <Glyph#> <Name>\n");
    go_die(1,"Bad Command Line");
    }

  rstrcpy(buf, site.run);
  strcat(buf, ".wrn");
  if((wrn.open(buf,WRITE))== 0)
  {
    fprintf(stderr,"Cannot open warning file %s\n", buf);
    go_die(3, "dumm");
  }
  wrn.write(dateoutst());
  wrn.write(timeoutst());
  wrn.writeln();
  wrn.close();

  return;
  }

void mem_out( )
  {
  go_die(2,"Insufficient Memory Available");
  }

void go_die(int mode, char *name)
  {
  if ( mode < 3 )
    {
    err.write(name);
    err.close();
    fprintf(stderr,"%s\n",name);
    exit(EXIT_FAILURE);
    }
  if ( mode == 3 )
    {
    err.close();
    exit(EXIT_FAILURE);
    }
  }

void WriteWarning(char *s)
{
  rstrcpy(buf, site.run);
  strcat(buf, ".wrn");
  if (wrn.open(buf,APPEND)!=0) return;
  wrn.write(s);
  wrn.writeln();
  wrn.close();
}



