#include <stdio.h>
#include <dir.h>

    int Copy(char *s,char *d)
      {
        long int BLOCK=32768;
        char *line;
        int t;

        line=new char[BLOCK];
        FILE *in,*out;
        in=fopen(s,"rb");
        if (in==NULL) return 0;
        out=fopen(d,"wb");
        do
          {
            t=fread(line,sizeof(char),BLOCK,in);
            fwrite(line,sizeof(char),t,out);
          }
        while (t==BLOCK-1);
        delete[] line;
        fclose(in);
        fclose(out);
        return 1;
      }

    int CopyAll(char *TempLoc,char *PermLoc)
      {
/*
        char current[MAXPATH];
        sprintf(current,"%s\*.*",TempLoc);
        spawnlp(P_WAIT,"copy","xcopy.exe","/Y",TempLoc,PermLoc,NULL);
*/
        struct ffblk ffblk;
        int done;
        char current[MAXPATH],dest[MAXPATH];
        char dummy[MAXPATH],file[MAXFILE],ext[MAXFILE];
        sprintf(current,"%s.*",TempLoc);
        done=findfirst(current,&ffblk,0);
        while (!done)
          {
            fnsplit(ffblk.ff_name,dummy,dummy,file,ext);
            sprintf(current,"%s%s",TempLoc,ext);
            sprintf(dest,"%s%s",PermLoc,ext);
            if (!Copy(current,dest))
              return 0;
            done=findnext(&ffblk);
          }
        findclose(&ffblk);
        return 1;

      }
