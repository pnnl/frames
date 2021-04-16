/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include <process.h>
#include <windows.h>
#include "frames.h"
#include "equation.h"
#include "sum3.h"

char *keyname = "LATINKEY";

static SUM *sum = NULL;

int CopyCase(char *s,char *d)
{
// s = .gid source filename
// d = destination filename - no extension
// no wildcards accepted
  int done;
  char npath[MAXPATH];
  char ndrive[MAXDRIVE];
  char ndir[MAXDIR];
  char nfile[MAXFILE];

  char drive[MAXDRIVE];
  char dir[MAXDIR];
  struct MY_fstruct fblk;

  if (!rstrcmpi(s,d)) return 0;
  fnflags = rfnsplit(d,ndrive,ndir,nfile,fnext);
  if (fnflags & WILDCARDS || !(fnflags & FILENAME) || fnflags & EXTENSION)
    return 0;
  fnflags = rfnsplit(s,drive,dir,fnfile,fnext);
  if (fnflags & WILDCARDS || !(fnflags & FILENAME) || !(fnflags & EXTENSION))
    return 0;
  if (rstrcmpi(fnext,".gid"))
    return 0;
  //should also check existance
  fnmerge(fnpath,drive,dir,fnfile,".*");
  done = findfirst(fnpath,&fblk,0);
  while (!done)
  {
    fnflags = rfnsplit(fblk.ff_name,fndrive,fndir,fnfile,fnext);
    fnmerge(fnpath,drive,dir,fnfile,fnext);
    fnmerge(npath,ndrive,ndir,nfile,fnext);
    if (!Copy(fnpath,npath))
      return 0;
    done = findnext(&fblk);
  }
  findclose(&fblk);
  return 1;
}
//------------------------------------------------------------------------------
int DelCase(char *s)
{
// s = .gid source filename
// no wildcards accepted
  int done;
  char drive[MAXDRIVE];
  char dir[MAXDIR];
  struct MY_fstruct fblk;

  fnflags = rfnsplit(s,drive,dir,fnfile,fnext);
  if (fnflags & WILDCARDS || !(fnflags & FILENAME) || !(fnflags & EXTENSION))
    return 0;
  if (rstrcmpi(fnext,".gid"))
    return 0;
  //should also check existance
  fnmerge(fnpath,drive,dir,fnfile,".*");
  done = findfirst(fnpath,&fblk,0);
  while (!done)
  {
    fnflags = rfnsplit(fblk.ff_name,fndrive,fndir,fnfile,fnext);
    fnmerge(fnpath,drive,dir,fnfile,fnext);
    unlink(fnpath);
    done = findnext(&fblk);
  }
  findclose(&fblk);
  return 1;
}

//------------------------------------------------------------------------------
int SUM::LaunchProcess(char *cmdline, char *apppath)
{
  int rc;
  rc = spawnlp (P_WAIT, apppath, cmdline, NULL);
  return rc;
}
//------------------------------------------------------------------------------
int SUM::writeId()
{
  FILE *f;
  char tmp[MAXPATH];

  sprintf(tmp,"%s%s",AppPath,"sens.id");
  f = fopen(tmp,"wt");
  if (f != NULL)
  {
    fprintf(f,"%s\n",keyname);
    fprintf(f,"%s\n",keyname);
    fprintf(f,"1\n");
    fprintf(f,"1\n");
    fclose(f);
    return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
int SUM::writeKey()
{
  FILE *k;
  char *Space = "/ ";
  char tmp[MAXPATH];

  // delete latin files in frames directory
  sprintf(tmp,"%s%s.LER",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.KEY",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.LHO",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.LRP",AppPath,keyname); unlink(tmp);

  if (!writeId()) return 0;
  sprintf(tmp,"%s%s.KEY",AppPath,keyname);
  k = fopen(tmp, "wt");
  if (k == NULL) return 0;
  sprintf(tmp,"\ %s",keyname);
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "/       Input file written by SUMMM at %s on %s", Time(), Date());
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", "/ Program: FRAMES 1.6 version ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/    This program is experimental and has not been            ");
  fprintf(k, "%-121s\n", "/    formally tested according to project procedures.         ");
  fprintf(k, "%-121s\n", "/    All results are preliminary in nature.                   ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/                          Review Signatures ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "/ Input Prepared By: ________________________________ Date: _______________  ");
  fprintf(k, "%-121s\n", "/ Input Reviewed By: ________________________________ Date: _______________  ");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "TITLE \"Run Name %s\"",keyname);
  fprintf(k, "%-121s\n", tmp);
  sprintf(tmp, "USER \"FRAMES USER\"");
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp, "ITERATE %d",Data->Iteration);
  fprintf(k, "%-121s\n", tmp);
  sprintf(tmp, "SEED %E",Data->Seed);
  fprintf(k, "%-121s\n", tmp);
  fprintf(k, "%-121s\n", Space);
  sprintf(tmp,"FILE REPORT \"%s.LRP\"",keyname);
  fprintf(k, "%-121s\n",tmp);
  sprintf(tmp,"FILE DATA   \"%s.LHO\"",keyname);
  fprintf(k, "%-121s\n",tmp);
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "OUTPUT a DATA table");
  fprintf(k, "%-121s\n", Space);
  fprintf(k, "%-121s\n", "OUTPUT the CORRELATION matrix");
  fprintf(k, "%-121s\n", Space);
  if (!Data->writeKey(k)) return 0;
  if (!Data->writeCorMat(k)) return 0;
  fprintf(k, "%-121s\n", "EXECUTE");
  fprintf(k, "%-121s\n", "END");
  fclose(k);
  return 1;
}
//------------------------------------------------------------------------------
int SUM::spawnLatin()
{
  int i;
  char tmp[MAXSTRING];
  struct ffblk ffblk;

  // Latin has no commandline interface
  sprintf(tmp,"%s%s",AppPath,"latin.exe");
  if (LaunchProcess(tmp, tmp))
  {
    writeError("Error launching latin.exe in SUM::spawnLatin!");
    return 0;
  }
  sprintf(tmp,"%s%s.LHO",AppPath,keyname);
  i = findfirst(tmp,&ffblk,0);
  findclose(&ffblk);
  if (i)
  {
    writeError("Error running latin in SUM::spawnLatin!");
    writeError("LHO file not found!");
    return 0;
  }

  sprintf(tmp,"%s%s.LER",AppPath,keyname);
  FILE *fle = fopen(tmp, "r");
  fseek(fle,0L,SEEK_END);
  if (ftell(fle) > 82)
  {
    writeError("Error running latin in SUM::spawnLatin!");
    fseek(fle,0L,SEEK_SET);
    do
    {
      fgets(tmp,MAXSTRING,fle);
      tmp[strlen(tmp)-1] = 0;
      writeError(tmp);
    }
    while (!feof(fle));
    fclose(fle);
    return 0;
  }
  fclose(fle);

  return 1;
}
//------------------------------------------------------------------------------
int SUM::spawnDoAll()
{
  int i;
  char tmp[MAXPATH];
  struct ffblk ffblk;

  sprintf(tmp,"%s %s %s %s %d",DoAllPath,ModId,sensin,sensout,SiteIdx-1);
  if (LaunchProcess(tmp, DoAllPath))
  {
    writeError("Error launching DoAll.exe in SUM::spawnDoAll!");
    return 0;
  }
  sprintf(tmp,"%s.err",sensout);
  i = !findfirst(tmp,&ffblk,0);
  findclose(&ffblk);
  if (i)
  {
    // may want to copy the contents of this file to the SUMM error file
    unlink(tmp);
    writeError("Error running case in SUM::spawnDoAll!");
    return 0;
  }
  return 1;
}
//------------------------------------------------------------------------------
int SUM::Initialize(char *argv0, char *argv1, char *argv2, char *argv3, char *argv4, char *argv5)
{
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//   chdir("C:\\FRAMES1.6");
//   chdir("C:\\FRAMESv1");
//  sprintf(AppPath,"%s\\FramesUI.ini",argv0);

//  argv0 must have short path and \\ at the end
  sprintf(AppPath,"%s\\",argv0);

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
  // Set arguments from FUI
  rstrcpy(FUIName,argv1);
  rstrcpy(RunName,argv2);
  SiteIdx = ratoi(argv3);
  ModIdx = ratoi(argv4);
  rstrcpy(ModId,argv5);
  // Set input/output filename for FUI
  sprintf(GIDName,"%s.gid",FUIName); // input GID file from FUI
  sprintf(MSGName,"%s.msg",RunName);
  sprintf(WRNName,"%s.wrn",RunName);
  sprintf(ERRName,"%s.err",RunName);
  sprintf(SUFName,"%s.suf",RunName); // output SUMMM file
  // Set working filenames for SUMMMM
  fnsplit(FUIName,fndrive,fndir,fnfile,fnext);
  fnmerge(sensin,fndrive,fndir,"~sens","");
  fnmerge(sensout,fndrive,fndir,"~senso","");
  fnmerge(workdir,fndrive,fndir,"","");
//  GetPrivateProfileString("App Path", "FUIshort", "", AppPath, MAXPATH, AppPath);
//  GetPrivateProfileString("App Path", "FUIshort", "", AppPath, MAXPATH, "FramesUI.ini");
  sprintf(DoAllPath,"%sDoAll.exe",AppPath);

  writeMsg("Initializing SUMMM data", "SUM::Initialize");
  Data = new SUMData();
  Ok = (Data != NULL);
  if (!Ok) writeError("Error initializing distribution data!", "SUM::Initialize");
  if (Ok)
  {
     writeMsg("Copying temporary case files", "SUM::Initialize");
     Ok = CopyCase(AddExtension(FUIName,"gid"),sensin);
  }
  if (Ok)
  {
    writeMsg("Creating SUF output file", "SUM::Initialize");
    SUF.open(SUFName,WRITE);
//    Ok = SUF.Ok();
    SUF.close();
    if (!Ok) writeError("Error opening SUF file!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Opening GID file", "SUM::Initialize");
    GID = Open_GID(GIDName);
    Ok = (GID != NULL);
    if (!Ok) writeError("Error opening GID file!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Loading GID file", "SUM::Initialize");
    Ok = Load_GID(GID,SiteIdx,ModId);
    if (!Ok) writeError("Error loading GID file!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Reading user distributions for key file", "SUM::Initialize");
    Ok = Data->read(GID,SiteIdx);
    if (!Ok) writeError("Error reading distribution data!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Closeing GID file", "SUM::Initialize");
    Close_GID(GID);
    if (!Ok) writeError("Error closeing GID file!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Writing user distributions to key file", "SUM::Initialize");
    Ok = writeKey();
    if (!Ok) writeError("Error writing distribution data!", "SUM::Initialize");
  }
  if (Ok)
  {
    writeMsg("Spawning Latin.exe to sample distributions", "SUM::Initialize");
    Ok = spawnLatin();
  }
  if (Ok) return Data->Iteration;
  return 0;
}
//------------------------------------------------------------------------------
int SUM::writeGID()
{
  char tmp[MAXPATH];
  char *line1,*line2,*line3;

  sprintf(tmp,"%s%s.lho",AppPath,keyname);
  Data->readLHO(tmp,Current);
  if (!Equation(Data,&line1,&line2,&line3))
  {
    writeError(line1,line2,line3);
    return 0;
  }
  sprintf(tmp,"%s.gid",sensin);
  Data->writeTempGID(GIDName,tmp);
  return 1;
}
//------------------------------------------------------------------------------
int SUM::Iterate(int iteration)
{
  int done;
  char tmp[MAXPATH];
  struct ffblk ffblk;

  Ok = (Data != NULL);
  if (!Ok) writeError("Error distribution data not initalized!","SUM::Iterate");

  //delete all temporary output files (~senso) to prepare for new output
  writeMsg("Deleting temporary files","SUM::Iterate");
  sprintf(tmp,"%s.*",sensout);
  done = findfirst(tmp,&ffblk,0);
  while (!done)
  {
    sprintf(tmp,"%s%s",workdir,ffblk.ff_name);
    unlink(tmp);
    done = findnext(&ffblk);
  }
  findclose(&ffblk);
  if (Ok)
  {
    Current = iteration-1;
    writeMsg("Writing substitute GID file","SUM::Iterate");
    Ok = writeGID();
    if (!Ok) writeError("Error writing substitute GID!", "SUM::Iterate");
  }
  if (Ok)
  {
    writeMsg("Spawning DoAll.exe on substitute GID file","SUM::Iterate");
    Ok = spawnDoAll();
  }
  if (Ok)
  {
    writeMsg("Reading S/U results in","SUM::Iterate");
    Ok = Data->readResults(sensin);
    if (!Ok) writeError("Error reading S/U results","SUM::Iterate");
  }
  writeMsg("Writing S/U results out","SUM::Iterate");
  SUF.open(SUFName,APPEND);
  Data->writeSUF(&SUF,Current,Count,!Ok);
  SUF.close();
//  Data->cleanup();
  return Ok;
}
//------------------------------------------------------------------------------
int SUM::Finalize()
{
  char tmp[MAXPATH];

  DelCase(AddExtension(sensin,"gid"));
  // delete latin files in application directory
  sprintf(tmp,"%s%s.KEY",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.LHO",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.LRP",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%s%s.LER",AppPath,keyname); unlink(tmp);
  sprintf(tmp,"%ssens.id",AppPath); unlink(tmp);

  delete Data;
  return Ok;
}

//------------------------------------------------------------------------------
// SUM API ---------------------------------------------------------------------
//------------------------------------------------------------------------------

int FRAMES_API Initialize(char *argv0, char *argv1, char *argv2, char *argv3, char *argv4, char *argv5)
{
  if (sum) delete sum;
  sum = new SUM();
  int ret = sum->Initialize(argv0, argv1, argv2, argv3, argv4, argv5);
  return ret;
}
//------------------------------------------------------------------------------
int FRAMES_API Iterate(int iteration)
{
  int ret = sum->Iterate(iteration);
  return ret;
}
//------------------------------------------------------------------------------
int FRAMES_API Finalize()
{
  int ret = sum->Finalize();
  if (sum) delete sum;
  sum = NULL;
  return ret;
}
