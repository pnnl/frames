#ifndef gidClassH
#define gidClassH
#include "csv.h"

//------------------------------------------------------------------------------
class Entry
{
        private:
                static char temp[256];
        public:
                long idx[6];
                long reference;
                char *usrunit;
                char *modunit;
                char *value;
                Entry *prev;
                Entry *next;
                Entry();
                ~Entry();
                Equal(long *index);
                void UsrUnitDup(char *val);
                void ModUnitDup(char *val);
                void ValueDup(char *val);
                void ValueDup(long val);
                void ValueDup(double val);
                int Long();
                double Double();
                int Read(icsv *in);
                int Write(ocsv *out);
};
//------------------------------------------------------------------------------
class Parameter
{
        private:
                static char temp[256];
        public:
                long count;
                char *name;
                Entry *List;
                Parameter *prev;
                Parameter *next;
                Parameter();
                ~Parameter();
                int AddEntry(Entry *e);
                int DelEntry(long *idx);
                Entry *GetEntry(long *idx);
                Entry *FindEntry(char *str);
                int Read(icsv *in);
                int Write(ocsv *out);
};
//------------------------------------------------------------------------------
class Section
{
        private:
                static char temp[256];
                Parameter *List;
        public:
                long fpos;
                long count;
                char *name;
                Section *prev;
                Section *next;
                Section();
                ~Section();
                int AddParameter(Parameter *p);
                int DelParameter(char *name);
                Parameter *GetParameter(char *name);
                long GetCount();
                int Read(icsv *in);
                int Write(ocsv *out);
};
//------------------------------------------------------------------------------
class GIDfile
{
        private:
                icsv *in;
                ocsv *out;
                static char temp[256];
                Section *List;
        public:
                char *name;
                int numsects;
                GIDfile(char *name);
                ~GIDfile();
                int AddSection(Section *s);
                int DelSection(char *name);
                Section *GetSection(char *name);
                Section *GetSection(int index);
                int WriteExcept(char *sectname);
                int Read();
                int Write();
};
//------------------------------------------------------------------------------
#endif
