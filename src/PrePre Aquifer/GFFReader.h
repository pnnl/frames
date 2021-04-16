//---------------------------------------------------------------------------

#ifndef GFFReaderH
#define GFFReaderH
//---------------------------------------------------------------------------

#include "csv.h"
//---------------------------------------------------------------------------
class GFFProg
{
        public:
                char name[64];
                char cas[64];
                char timeunit[64];
                char fluxunit[64];
                int numfluxpairs;
                int numfluxtypes;
                char parentname[64];
                char parentcas[64];
                double *times;
                double *flux1;
                double *flux2;

                GFFProg();
                ~GFFProg();
                void Read(icsv *in);
                void Write(FILE *out);
};
//---------------------------------------------------------------------------
class GFFContam
{
        public:
                char name[64];
                char cas[64];
                char timeunit[64];
                char fluxunit[64];
                int numfluxpairs;
                int numfluxtypes;
                int numprog;
                double *times;
                double *flux1;
                double *flux2;
                GFFProg **prog;

                GFFContam();
                ~GFFContam();
                void Read(icsv *in);
                void Write(FILE *out);
};
//---------------------------------------------------------------------------
class GFFData
{
        public:
                char name[64];
                char medtype[64];
                double width;
                char wunit[64];
                double length;
                char lunit[64];
                double distance;
                char dunit[64];
                double recharge;
                char runit[64];
                int numcon;
                char fluxtimeunit[64];
                char fluxunit[64];
                int numwflux;
                double *times;
                double *fluxes;
                int numvertices;
                double *x;
                double *y;
                double *z;
                GFFContam **cons;

                GFFData();
                ~GFFData();
                void Read(icsv *in);
                void Write(FILE *out);
                void WriteWFFData(FILE *out);
};
//---------------------------------------------------------------------------
class GFFMod
{
        public:
                char name[64];
                int numlines;
                int numheader;
                char headers[128][128];
                int numsets;
                GFFData **moddata;

                GFFMod();
                ~GFFMod();
                void Read(icsv *in);
                void Write(FILE *out);
                void WriteWFFModule(FILE *out);
};
//---------------------------------------------------------------------------
class GFFfile
{
        private:
                icsv *in;
                FILE *out;
        public:
                int nummod;
                GFFMod **module;
                GFFfile();
                ~GFFfile();
                GFFMod *GetModule(char *moduleId);
                int Read(char *fuiname);
                int Write(char *fuiname);
                int WriteWFFSection(char *fuiname,char *sectname);
};
#endif
