//---------------------------------------------------------------------------

#pragma hdrstop

#include "GFFReader.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
GFFProg::GFFProg()
{
        numfluxpairs = 0;
        numfluxtypes = 0;
        times = NULL;
        flux1 = NULL;
        flux2 = NULL;
}
//---------------------------------------------------------------------------
GFFProg::~GFFProg()
{
        if(times)
                delete[] times;
        if(flux1)
                delete[] flux1;
        if(flux2)
                delete[] flux2;
}
//---------------------------------------------------------------------------
void GFFProg::Read(icsv *in)
{
        int i;
        *in >> name >> cas >> timeunit >> fluxunit >> numfluxpairs >> numfluxtypes;
        *in >> parentname >> parentcas >> NewLn;
        times = new double[numfluxpairs];
        flux1 = new double[numfluxpairs];
        flux2 = new double[numfluxpairs];
        for(i=0;i<numfluxpairs;i++)
        {
                *in >> times[i];
                *in >> flux1[i];
                if(numfluxtypes == 2)
                        *in >> flux2[i];
                else
                        flux2[i] = -999.0;
                *in >> NewLn;
        }
}
//---------------------------------------------------------------------------
void GFFProg::Write(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",\"%s\",\"%s\",\"%s\",%d,%d,\"%s\",\"%s\"\n",name,cas,timeunit,fluxunit,numfluxpairs,
                numfluxtypes,parentname,parentcas);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numfluxpairs;i++)
        {
                sprintf(temp,"%2.7E,%2.7E",times[i],flux1[i]);
                fwrite(temp,strlen(temp),1,out);
                if(numfluxtypes == 2)
                {
                        sprintf(temp,",%2.7E",flux2[i]);
                        fwrite(temp,strlen(temp),1,out);
                }
                fwrite("\n",1,1,out);
        }
}
//---------------------------------------------------------------------------
GFFContam::GFFContam()
{
        numfluxpairs = 0;
        numfluxtypes = 0;
        numprog = 0;
        times = NULL;
        flux1 = NULL;
        flux2 = NULL;
        prog = NULL;
}
//---------------------------------------------------------------------------
GFFContam::~GFFContam()
{
        if(times)
                delete[] times;
        if(flux1)
                delete[] flux1;
        if(flux2)
                delete[] flux2;
        if(prog)
                delete[] prog;
}
//---------------------------------------------------------------------------
void GFFContam::Read(icsv *in)
{
        int i;
        *in >> name >> cas >> timeunit >> fluxunit >> numfluxpairs;
        *in >> numfluxtypes >> numprog >> NewLn;
        times = new double[numfluxpairs];
        flux1 = new double[numfluxpairs];
        flux2 = new double[numfluxpairs];
        for(i=0;i<numfluxpairs;i++)
        {
                *in >> times[i];
                *in >> flux1[i];
                if(numfluxtypes == 2)
                        *in >> flux2[i];
                else
                        flux2[i] = -999.0;
                *in >> NewLn;
        }
        prog = new GFFProg*[numprog];
        for(i=0;i<numprog;i++)
        {
                prog[i] = new GFFProg();
                prog[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void GFFContam::Write(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",\"%s\",\"%s\",\"%s\",%d,%d,%d\n",name,cas,timeunit,fluxunit,
                numfluxpairs,numfluxtypes,numprog);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numfluxpairs;i++)
        {
                sprintf(temp,"%2.7E,%2.7E",times[i],flux1[i]);
                fwrite(temp,strlen(temp),1,out);
                if(numfluxtypes == 2)
                {
                        sprintf(temp,",%2.7E",flux2[i]);
                        fwrite(temp,strlen(temp),1,out);
                }
                fwrite("\n",1,1,out);
        }
        for(i=0;i<numprog;i++)
        {
                prog[i]->Write(out);
        }
}
//---------------------------------------------------------------------------
GFFData::GFFData()
{
        width = 0;
        length = 0;
        distance = 0;
        recharge = 0;
        numcon = 0;
        numwflux = 0;
        times = NULL;
        fluxes = NULL;
        cons = NULL;
        numvertices = 0;
        x = NULL;
        y = NULL;
        z = NULL;
}
//---------------------------------------------------------------------------
GFFData::~GFFData()
{
        if(times)
                delete[] times;
        if(fluxes)
                delete[] fluxes;
        if(cons)
                delete[] cons;
        if(x)
                delete[] x;
        if(y)
                delete[] y;
        if(z)
                delete[] z;
}
//---------------------------------------------------------------------------
void GFFData::Read(icsv *in)
{
        int i;
        *in >> name >> medtype >> width >> wunit >> length >> lunit;
        *in >> distance >> dunit >> recharge >> runit >> numcon >> NewLn;
        *in >> numvertices >> NewLn;
        x = new double[numvertices];
        y = new double[numvertices];
        z = new double[numvertices];
        for(i=0;i<numvertices;i++)
                *in >> x[i] >> y[i] >> z[i] >> NewLn;
        *in >> fluxtimeunit >> fluxunit >> numwflux >> NewLn;
        times = new double[numwflux];
        fluxes = new double[numwflux];
        for(i=0;i<numwflux;i++)
        {
                *in >> times[i];
                *in >> fluxes[i];
                *in >> NewLn;
        }
        cons = new GFFContam*[numcon];
        for(i=0;i<numcon;i++)
        {
                cons[i] = new GFFContam();
                cons[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void GFFData::Write(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%d\n",
        name,medtype,width,wunit,length,lunit,distance,dunit,recharge,
        runit,numcon);
        fwrite(temp,strlen(temp),1,out);

        sprintf(temp,"%d\n",numvertices);
        for(i=0;i<numvertices;i++)
                sprintf(temp,"%2.7E,2.7E,2.7E\n",x[i],y[i],z[i]);
        sprintf(temp,"\"%s\",\"%s\",%d\n",fluxtimeunit,fluxunit,numwflux);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numwflux;i++)
        {
                sprintf(temp,"%2.7E,%2.7E\n",times[i],fluxes[i]);
                fwrite(temp,strlen(temp),1,out);
        }
        for(i=0;i<numcon;i++)
        {
                cons[i]->Write(out);
        }
}
//---------------------------------------------------------------------------
void GFFData::WriteWFFData(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%2.7E,\"%s\",%d\n",
        name,medtype,width,wunit,length,lunit,distance,dunit,recharge,
        runit,numcon);
        fwrite(temp,strlen(temp),1,out);
        sprintf(temp,"\"%s\",\"%s\",%d\n",fluxtimeunit,fluxunit,numwflux);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numwflux;i++)
        {
                sprintf(temp,"%2.7E,%2.7E\n",times[i],fluxes[i]);
                fwrite(temp,strlen(temp),1,out);
        }
        for(i=0;i<numcon;i++)
        {
                cons[i]->Write(out);
        }
}
//---------------------------------------------------------------------------
GFFMod::GFFMod()
{
        numlines = 0;
        numheader = 0;
        numsets = 0;
        moddata = NULL;
}
//---------------------------------------------------------------------------
GFFMod::~GFFMod()
{
        if(moddata)
                delete[] moddata;
}
//---------------------------------------------------------------------------
void GFFMod::Read(icsv *in)
{
        int i;
        *in >> name >> numlines >> NewLn;
        *in >> numheader >> NewLn;
        for(i=0;i<numheader;i++)
        {
                *in >> headers[i] >> NewLn;
        }
        *in >> numsets >> NewLn;
        moddata = new GFFData*[numsets];
        for(i=0;i<numsets;i++)
        {
                moddata[i] = new GFFData();
                moddata[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void GFFMod::Write(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",%d\n",name,numlines);
        fwrite(temp,strlen(temp),1,out);
        sprintf(temp,"%d\n",numheader);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numheader;i++)
        {
                sprintf(temp,"\"%s\"\n",headers[i]);
                fwrite(temp,strlen(temp),1,out);
        }
        sprintf(temp,"%d\n",numsets);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numsets;i++)
        {
                moddata[i]->Write(out);
        }
}
//---------------------------------------------------------------------------
void GFFMod::WriteWFFModule(FILE *out)
{
        int i;
        char temp[1024];
        sprintf(temp,"\"%s\",%d\n",name,numlines);
        fwrite(temp,strlen(temp),1,out);
        sprintf(temp,"%d\n",numheader);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numheader;i++)
        {
                sprintf(temp,"\"%s\"\n",headers[i]);
                fwrite(temp,strlen(temp),1,out);
        }
        sprintf(temp,"%d\n",numsets);
        fwrite(temp,strlen(temp),1,out);
        for(i=0;i<numsets;i++)
        {
                moddata[i]->WriteWFFData(out);
        }
}
//---------------------------------------------------------------------------
GFFfile::GFFfile()
{
        nummod = 0;
        in = NULL;
        out = NULL;
        module = NULL;
}
//---------------------------------------------------------------------------
GFFfile::~GFFfile()
{
        if(in)
                delete in;
        if(out)
                delete out;
        if(module)
                delete[] module;
}
//---------------------------------------------------------------------------
int GFFfile::Read(char *fuiname)
{
        char inpath[256];
        char modname[64];
        int numlines;
        int i;
        sprintf(inpath,"%s.gff",fuiname);
        in = new icsv(inpath);
        if(!in->ok())
                return 0;
        while(!in->eof())
        {
                nummod++;
                *in >> modname >> numlines >> NewLn;
                in->Skip(numlines);
        }
        delete in;
        in = new icsv(inpath);
        module = new GFFMod*[nummod];
        for(i=0;i<nummod;i++)
        {
                module[i] = new GFFMod();
                module[i]->Read(in);
        }
        return 1;
}
//---------------------------------------------------------------------------
int GFFfile::Write(char *fuiname)
{
        char inpath[256];
        int i;
        sprintf(inpath,"%s.gff",fuiname);
        out = fopen(inpath,"w");
        if(!out)
                return 0;
        for(i=0;i<nummod-1;i++)
                module[i]->Write(out);
        fclose(out);
        return 1;
}
//---------------------------------------------------------------------------
int GFFfile::WriteWFFSection(char *fuiname,char *sectname)
{
        char inpath[256];
        int i;
        sprintf(inpath,"%s.wff",fuiname);
        out = fopen(inpath,"a");
        if(!out)
                return 0;
        for(i=0;i<nummod-1;i++)
        {
                if(strcmpi(module[i]->name,sectname)==0)
                        module[i]->WriteWFFModule(out);
        }
        fclose(out);
        return 1;
}
