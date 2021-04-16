//---------------------------------------------------------------------------

#ifndef WFFReaderH
#define WFFReaderH
#include "csv.h"
//---------------------------------------------------------------------------
class WFFProg
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

                WFFProg();
                ~WFFProg();
                void Read(icsv *in);
//                void Write(ocsv *out);
                void Write(FILE *out);
};
//---------------------------------------------------------------------------
class WFFContam
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
                WFFProg **prog;

                WFFContam();
                ~WFFContam();
                void Read(icsv *in);
//                void Write(ocsv *out);
                void Write(FILE *out);
};
//---------------------------------------------------------------------------
class WFFData
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
                WFFContam **cons;

                WFFData();
                ~WFFData();
                void Read(icsv *in);
//                void Write(ocsv *out);
                void Write(FILE *out);
};
//---------------------------------------------------------------------------
class WFFMod
{
        public:
                char name[64];
                int numlines;
                int numheader;
                char headers[128][128];
                int numsets;
                WFFData **moddata;

                WFFMod();
                ~WFFMod();
                void Read(icsv* in);
//                void Write(ocsv* out);
                void Write(FILE* out);
};
//---------------------------------------------------------------------------
class WFFfile
{
        private:
                icsv *in;
//                ocsv *out;
                FILE *out;
        public:
                int nummod;
                WFFMod **module;
                WFFfile();
                ~WFFfile();
                WFFMod *GetModule(char *moduleId);
                int Read(char *fuiname);
                int Write(char *fuiname);
                int WriteExcept(char *fuiname,char *sectname);
};
#endif

