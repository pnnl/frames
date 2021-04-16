//---------------------------------------------------------------------------

#pragma hdrstop

#include "WFFReader.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
WFFProg::WFFProg()
{
        numfluxpairs = 0;
        numfluxtypes = 0;
        times = NULL;
        flux1 = NULL;
        flux2 = NULL;
}
//---------------------------------------------------------------------------
WFFProg::~WFFProg()
{
        if(times)
                delete[] times;
        if(flux1)
                delete[] flux1;
        if(flux2)
                delete[] flux2;
}
//---------------------------------------------------------------------------
void WFFProg::Read(icsv *in)
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
void WFFProg::Write(FILE *out)
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
WFFContam::WFFContam()
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
WFFContam::~WFFContam()
{
        int i;
        if(times)
                delete[] times;
        if(flux1)
                delete[] flux1;
        if(flux2)
                delete[] flux2;
        if(prog)
        {
                for(i=0;i<numprog;i++)
                        delete prog[i];
                delete[] prog;
        }
}
//---------------------------------------------------------------------------
void WFFContam::Read(icsv *in)
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
        prog = new WFFProg*[numprog];
        for(i=0;i<numprog;i++)
        {
                prog[i] = new WFFProg();
                prog[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void WFFContam::Write(FILE *out)
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
WFFData::WFFData()
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
}
//---------------------------------------------------------------------------
WFFData::~WFFData()
{
        int i;
        if(times)
                delete[] times;
        if(fluxes)
                delete[] fluxes;
        if(cons)
        {
                for(i=0;i<numcon;i++)
                        delete cons[i];
                delete[] cons;
        }
}
//---------------------------------------------------------------------------
void WFFData::Read(icsv *in)
{
        int i;
        *in >> name >> medtype >> width >> wunit >> length >> lunit;
        *in >> distance >> dunit >> recharge >> runit >> numcon >> NewLn;
        *in >> fluxtimeunit >> fluxunit >> numwflux >> NewLn;
        times = new double[numwflux];
        fluxes = new double[numwflux];
        for(i=0;i<numwflux;i++)
        {
                *in >> times[i];
                *in >> fluxes[i];
                *in >> NewLn;
        }
        cons = new WFFContam*[numcon];
        for(i=0;i<numcon;i++)
        {
                cons[i] = new WFFContam();
                cons[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void WFFData::Write(FILE *out)
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
WFFMod::WFFMod()
{
        numlines = 0;
        numheader = 0;
        numsets = 0;
        moddata = NULL;
}
//---------------------------------------------------------------------------
WFFMod::~WFFMod()
{
        int i;
        if(moddata)
        {
                for(i=0;i<numsets;i++)
                        delete moddata[i];
                delete[] moddata;
        }
}
//---------------------------------------------------------------------------
void WFFMod::Read(icsv *in)
{
        int i;
        *in >> name >> numlines >> NewLn;
        *in >> numheader >> NewLn;
        for(i=0;i<numheader;i++)
        {
                *in >> headers[i] >> NewLn;
        }
        *in >> numsets >> NewLn;
        moddata = new WFFData*[numsets];
        for(i=0;i<numsets;i++)
        {
                moddata[i] = new WFFData();
                moddata[i]->Read(in);
        }
}
//---------------------------------------------------------------------------
void WFFMod::Write(FILE *out)
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
WFFfile::WFFfile()
{
        nummod = 0;
        in = NULL;
        out = NULL;
        module = NULL;
}
//---------------------------------------------------------------------------
WFFfile::~WFFfile()
{
        int i;
        if(in)
                delete in;
        if(module)
        {
                for(i=0;i<nummod;i++)
                        delete module[i];
                delete[] module;
        }
}
//---------------------------------------------------------------------------
int WFFfile::Read(char *fuiname)
{
        char inpath[256];
        char modname[64];
        int numlines;
        int i;
        sprintf(inpath,"%s.wff",fuiname);
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
        module = new WFFMod*[nummod];
        for(i=0;i<nummod;i++)
        {
                module[i] = new WFFMod();
                module[i]->Read(in);
        }
        delete in;
        in = NULL;
        return 1;
}
//---------------------------------------------------------------------------
int WFFfile::Write(char *fuiname)
{
        char inpath[256];
        int i;
        sprintf(inpath,"%s.wff",fuiname);
        out = fopen(inpath,"w");
        if(!out)
                return 0;
        for(i=0;i<nummod-1;i++)
                module[i]->Write(out);
        fclose(out);
        return 1;
}
//---------------------------------------------------------------------------
int WFFfile::WriteExcept(char *fuiname,char *sectname)
{
        char inpath[256];
        int i;
        sprintf(inpath,"%s.wff",fuiname);
        out = fopen(inpath,"w");
        if(!out)
                return 0;
        for(i=0;i<nummod-1;i++)
                if(strcmpi(sectname,module[i]->name))
                        module[i]->Write(out);
        fclose(out);
        return 1;
}

