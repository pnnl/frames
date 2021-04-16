//------------------------------------------------------------------------------
#include "gid.h"
//------------------------------------------------------------------------------
char Entry::temp[256];
//------------------------------------------------------------------------------
Entry::Entry()
{
        for (int i=0; i<6; i++)
                idx[i] = 0;
        reference = 0;
        usrunit = NULL;
        modunit = NULL;
        value = NULL;
        prev = NULL;
        next = NULL;
}
//------------------------------------------------------------------------------
Entry::~Entry()
{
        if(value)
                delete[] value;
        if(usrunit)
                delete[] usrunit;
        if(modunit)
                delete[] modunit;
}
//------------------------------------------------------------------------------
Entry::Equal(long *index)
{
        for (int i=0; i<6; i++)
                if (idx[i] != index[i])
                        return false;
        return true;
}
//------------------------------------------------------------------------------
void Entry::UsrUnitDup(char *val)
{
        if (usrunit != NULL) delete usrunit;
        usrunit = strdup(val);
}
//------------------------------------------------------------------------------
void Entry::ModUnitDup(char *val)
{
        if (modunit != NULL) delete modunit;
        modunit = strdup(val);
}
//------------------------------------------------------------------------------
void Entry::ValueDup(char *val)
{
        if (value != NULL) delete value;
        value = strdup(val);
}
//------------------------------------------------------------------------------
void Entry::ValueDup(long val)
{
        if (value != NULL) delete value;
        sprintf(temp,"%ld",val);
        value = strdup(temp);
}
//------------------------------------------------------------------------------
void Entry::ValueDup(double val)
{
        if (value != NULL) delete value;
        sprintf(temp,"%lf",val);
        value = strdup(temp);
}
//------------------------------------------------------------------------------
int Entry::Long()
{
  return atol(value);
//        return ratol(value);
}
//------------------------------------------------------------------------------
double Entry::Double()
{
        char *stopString;
        return strtod((value),&stopString);
}
//------------------------------------------------------------------------------
int Entry::Read(icsv *in)
{
        if (in == NULL) return false;
        *in >> idx[0] >> idx[1] >> idx[2] >> idx[3] >> idx[4] >> idx[5] >> reference;
        *in >> temp;
        UsrUnitDup(temp);
        *in >> temp;
        ModUnitDup(temp);
        *in >> temp >> NewLn;
        ValueDup(temp);
        return true;
}
//------------------------------------------------------------------------------
int Entry::Write(ocsv *out)
{
        if (out == NULL) return false;
        *out << idx[0] << idx[1] << idx[2] << idx[3] << idx[4] << idx[5]
        << reference << usrunit << modunit << value << NewLn;
        return true;
}
//------------------------------------------------------------------------------
char Parameter::temp[256];
//------------------------------------------------------------------------------
Parameter::Parameter()
{
        count = 0;
        name = NULL;
        List = NULL;
        prev = NULL;
        next = NULL;
}
//------------------------------------------------------------------------------
Parameter::~Parameter()
{
        Entry *next, *curr = List;
        while (curr!=NULL)
        {
                next = curr->next;
                delete curr;
                curr = next;
        }
}
//------------------------------------------------------------------------------
// return 0 if already in List
// return 1 if inserted in List
int Parameter::AddEntry(Entry *e)
{
        if (GetEntry(e->idx) != NULL) return 0;
        if (List == NULL)
        {
                List = e;
                List->prev = e;
        }
        else
        {
                e->prev = List->prev;
                List->prev->next = e;
                List->prev = e;
        }
        this->count++;
        return 1;
}
//------------------------------------------------------------------------------
// return 0 if not in List
// return 1 if deleted from List
int Parameter::DelEntry(long *idx)
{
        Entry *e = GetEntry(idx);
        if (e == NULL) return 0;
        if (List == e)
                List = NULL;
        else
                e->prev->next = e->next;
        this->count--;
        return 1;
}
//------------------------------------------------------------------------------
Entry *Parameter::GetEntry(long *idx)
{
        Entry *curr = List;
        while (curr!=NULL)
        {
                if (curr->Equal(idx))
                return curr;
                curr = curr->next;
        }
        return NULL;
}
//------------------------------------------------------------------------------
Entry *Parameter::FindEntry(char *str)
{
        Entry *curr = List;
        while (curr!=NULL)
        {
                if (!rstrcmpi(curr->value,str))
                        return curr;
                curr = curr->next;
        }
        return NULL;
}
//------------------------------------------------------------------------------
int Parameter::Read(icsv *in)
{
        try
        {
                Entry *e = new Entry();
                e->Read(in);
                return AddEntry(e);
        }
        catch(...)
        {
                return false;
        }
}
//------------------------------------------------------------------------------
int Parameter::Write(ocsv *out)
{
        int i;
        if (out == NULL) return false;
        Entry *curr = List;
        while (curr!=NULL)
        {
                *out << name;
                for(i=0;i<6;i++)
                        *out << curr->idx[i];
                *out << curr->reference;
                *out << curr->usrunit;
                *out << curr->modunit;
                *out << curr->value;
                *out << NewLn;
                curr = curr->next;
        }
        return true;
}
//------------------------------------------------------------------------------
char Section::temp[256];
//------------------------------------------------------------------------------
Section::Section()
{
        this->count = 0;
        this->name = NULL;
        this->List = NULL;
        this->prev = NULL;
        this->next = NULL;
}
//------------------------------------------------------------------------------
Section::~Section()
{
        Parameter *next, *curr = List;
        while (curr!=NULL)
        {
                next = curr->next;
                delete curr;
                curr = next;
        }
}
//------------------------------------------------------------------------------
// return 0 if already in List
// return 1 if inserted in List
int Section::AddParameter(Parameter *p)
{
        if (GetParameter(p->name) != NULL) return 0;
        if (List == NULL)
        {
                List = p;
                List->prev = p;
        }
        else
        {
                p->prev = List->prev;
                List->prev->next = p;
                List->prev = p;
        }
//        count++;
        return 1;
}
//------------------------------------------------------------------------------
// return 0 if not in List
// return 1 if deleted from List
int Section::DelParameter(char *name)
{
        Parameter *p = GetParameter(name);
        if (p == NULL) return 0;
        if (List == p)
                List = NULL;
        else
        {
                if(p->prev)
                        p->prev->next = p->next;
                if(p->next)
                        p->next->prev = p->prev;
        }
        delete p;
        this->count--;
        return 1;
}
//------------------------------------------------------------------------------
Parameter *Section::GetParameter(char *name)
{
        Parameter *curr = List;
        while (curr!=NULL)
        {
                if (!rstrcmpi(curr->name,name))
                return curr;
                curr = curr->next;
        }
        return NULL;
}
//------------------------------------------------------------------------------
long Section::GetCount()
{
        long cnt = 0;
        Parameter *curr = List;
        while (curr!=NULL)
        {
                cnt+= curr->count;
                curr = curr->next;
        }
        return cnt;
}
//------------------------------------------------------------------------------
int Section::Read(icsv *in)
{
  int cnt = 0;
  Parameter *p;

        if (List != NULL) return true;
        in->setpos(fpos);
        for (long i=0; i<count; i++)
        {
                try
                {
                   p = new Parameter();
                }
                catch(...)
                {
                   count = cnt;
                   return false;
                }
                *in >> temp;
                p->name = strdup(temp);
                if (!AddParameter(p))
                {
                  delete p;
                  p = GetParameter(temp);
                }
                if (p->Read(in)) cnt++;
        }
        count = cnt;
        return true;
}
//------------------------------------------------------------------------------
int Section::Write(ocsv *out)
{
        if (out == NULL) return false;
//        *out << name << GetCount() << NewLn;
        *out << name << this->count << NewLn;
        Parameter *curr = List;
        while (curr!=NULL)
        {
                curr->Write(out);
                curr = curr->next;
        }
        return true;
}
//------------------------------------------------------------------------------
char GIDfile::temp[256];
//------------------------------------------------------------------------------
GIDfile::GIDfile(char *name)
{
        this->in = NULL;
        this->out = NULL;
        this->List = NULL;
        this->numsects = 0;
        this->name = strdup(name);
}
//------------------------------------------------------------------------------
GIDfile::~GIDfile()
{
        Section *next, *curr = List;
        while (curr!=NULL)
        {
                next = curr->next;
                delete curr;
                curr = next;
        }
//        if(in)
//                delete in;
//        if(out)
//                delete out;
}
//------------------------------------------------------------------------------
// return 0 if already in List
// return 1 if inserted in List
int GIDfile::AddSection(Section *s)
{
        if (GetSection(s->name) != NULL) return 0;
        if (List == NULL)
        {
                List = s;
                List->prev = s;
        }
        else
        {
                s->prev = List->prev;
                List->prev->next = s;
                List->prev = s;
        }
        this->numsects++;
        return 1;
}
//------------------------------------------------------------------------------
// return 0 if not in List
// return 1 if deleted from List
int GIDfile::DelSection(char *name)
{
        Section *s = GetSection(name);
        if (s == NULL) return 0;
        if (List == s)
                List = NULL;
        else
                s->prev->next = s->next;
        delete s;
        return 1;
}
//------------------------------------------------------------------------------
Section *GIDfile::GetSection(char *name)
{
        Section *curr = List;
        while (curr!=NULL)
        {
                if (!rstrcmpi(curr->name,name))
                {
                        curr->Read(in);
                        return curr;
                }
                curr = curr->next;
        }
        return NULL;
}
//------------------------------------------------------------------------------
Section *GIDfile::GetSection(int index)
{
        int i = 0;
        Section *curr = List;
        while (curr!=NULL)
        {
                if (i == index)
                {
                        curr->Read(in);
                        return curr;
                }
                curr = curr->next;
                i++;
        }
        return NULL;
}
//------------------------------------------------------------------------------
int GIDfile::Read()
{
  int tcnt;
  Section *s;

  try
  {
    in = new icsv(name);
  }
  catch(...)
  {
    return false;
  }
  while (!in->eof())
  {
    *in >> temp >> tcnt >> NewLn;

    try
    {
      s = new Section();
    }
    catch(...)
    {
      return false;
    }

    if (tcnt == 0) continue;
    s->fpos = in->getpos();
    s->name = strdup(temp);
    s->count = tcnt;
    for (int i=0; i<tcnt; i++) *in >> NewLn;
    if (!AddSection(s)) delete s;
  }
  return true;
}
//------------------------------------------------------------------------------
int GIDfile::Write()
{
        try
        {
                out = new ocsv(name);
                Section *curr = List;
                while (curr!=NULL)
                {
                        curr->Write(out);
                        curr = curr->next;
                }
                return true;
        }
        catch(...)
        {
                return false;
        }
}
//------------------------------------------------------------------------------
int GIDfile::WriteExcept(char *sectname)
{
        try
        {
                out = new ocsv(name);
                Section *curr = List;
                while(curr!=NULL)
                {
                        if(stricmp(sectname,curr->name)!=0)
                                curr->Write(out);
                        curr = curr->next;
                }
                return true;
        }
        catch(...)
        {
                return false;
        }
}
