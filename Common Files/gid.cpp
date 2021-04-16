/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________
  
    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "gid.h"
#define WILDCARDS 0x01
#define EXTENSION 0x02
#define FILENAME  0x04
#define DIRECTORY 0x08
#define DRIVE     0x10      // 16

//------------------------------------------------------------------------------
// used with read and write routines
paramrec val;
glyphrec glyph;

//------------------------------------------------------------------------------
char *getvalu(element *e, int c1,int c2,int c3,int c4,int c5,int c6)
{
  element *curr = e;
  while (curr != NULL)
  {
    if (c1 == curr->idx1 && c2 == curr->idx2 && c3 == curr->idx3 &&
      c4 == curr->idx4 && c5 == curr->idx5 && c6 == curr->idx6)
      return curr->valu;
    curr = curr->next;
  }
  return "-1.0";
}
//------------------------------------------------------------------------------
int findidx(element *e, char *valu, int idx)
{
  element *curr = e;
  while (curr != NULL)
  {
    if (!rstrcmpi(curr->valu,valu))
      switch (idx)
    {
        case 1: return curr->idx1;
        case 2: return curr->idx2;
        case 3: return curr->idx3;
        case 4: return curr->idx4;
        case 5: return curr->idx5;
        case 6: return curr->idx6;
    }
    curr = curr->next;
  }
  return -1;
}
//------------------------------------------------------------------------------
int readglyph(GIDFILE *f)
{
  if (f->f.eof())
    return 0;
  else
  {
    f->f.read(glyph.name);
    f->f.read(&glyph.cnt);
    f->f.readln();
    return glyph.cnt;
  }
}
//------------------------------------------------------------------------------
int writeglyph(GIDFILE *f)
{
  if (f->f.eof())
    return 0;
  else
  {
    f->f.write(glyph.name);
    f->f.write(glyph.cnt);
    f->f.writeln();
    return 1;
  }
}
//------------------------------------------------------------------------------
paramrec *readparamrec(GIDFILE *f)
{
  if (f->f.eof())
    return NULL;
  else
  {
    f->f.read(val.name);
    f->f.read(&val.cnt1);
    f->f.read(&val.cnt2);
    f->f.read(&val.cnt3);
    f->f.read(&val.cnt4);
    f->f.read(&val.cnt5);
    f->f.read(&val.cnt6);
    f->f.read(&val.ref);
    f->f.read(val.uunit);
    f->f.read(val.punit);
    f->f.read(val.valu);
    val.quote = f->f.wasQuoted;
    f->f.readln();
    return &val;
  }
}
//------------------------------------------------------------------------------
void writeparamrec(GIDFILE *f, paramrec *p)
{
  p->name[SMALLSTRING-1] = '\0';
  p->uunit[SMALLSTRING-1] = '\0';
  p->punit[SMALLSTRING-1] = '\0';
  p->valu[LARGESTRING-1] = '\0';
  f->f.write(p->name);
  f->f.write(p->cnt1);
  f->f.write(p->cnt2);
  f->f.write(p->cnt3);
  f->f.write(p->cnt4);
  f->f.write(p->cnt5);
  f->f.write(p->cnt6);
  f->f.write(p->ref);
  f->f.write(p->uunit);
  f->f.write(p->punit);
  char *s=p->valu;
  if (!val.quote)
  {
    while (*s!=',' && *s!='\0') s++;
    f->f.write(p->valu,*s==',');
  }
  else
    f->f.write(p->valu,true);
  f->f.writeln();
}
//------------------------------------------------------------------------------
void copyparamrec(paramrec *d,paramrec *s)
{
  int i;
  
  d->cnt1 = s->cnt1;
  d->cnt2 = s->cnt2;
  d->cnt3 = s->cnt3;
  d->cnt4 = s->cnt4;
  d->cnt5 = s->cnt5;
  d->cnt6 = s->cnt6;
  d->ref  = s->ref;
  for (i=0; i<LARGESTRING; i++)    d->valu[i] = s->valu[i];
  for (i=0; i<SMALLSTRING; i++)
  {
    d->name[i]  = s->name[i];
    d->uunit[i] = s->uunit[i];
    d->punit[i] = s->punit[i];
    d->fmt[i]   = s->fmt[i];
  }
}
//------------------------------------------------------------------------------
void parm2entry(paramrec *s,element *d)
{
  int i;
  
  d->idx1 = s->cnt1;
  d->idx2 = s->cnt2;
  d->idx3 = s->cnt3;
  d->idx4 = s->cnt4;
  d->idx5 = s->cnt5;
  d->idx6 = s->cnt6;
  d->ref = s->ref;
  for (i=0; i<LARGESTRING; i++)    d->valu[i] = s->valu[i];
}
//------------------------------------------------------------------------------
int idxequal(paramrec *f,element *s)
{
  int val;
  if (s==NULL) return 0;
  if (f==NULL) return 0;
  val=(f->cnt1 == s->idx1 &&
    f->cnt2 == s->idx2 &&
    f->cnt3 == s->idx3 &&
    f->cnt4 == s->idx4 &&
    f->cnt5 == s->idx5 &&
    f->cnt6 == s->idx6);
  return val;
}
//------------------------------------------------------------------------------
int paramequal(paramrec *f,paramrec *s)
{
  int val;
  
  val=(!rstrcmpi(f->name,s->name) &&
    f->cnt1 == s->cnt1 &&
    f->cnt2 == s->cnt2 &&
    f->cnt3 == s->cnt3 &&
    f->cnt4 == s->cnt4 &&
    f->cnt5 == s->cnt5 &&
    f->cnt6 == s->cnt6);
  return val;
}
//------------------------------------------------------------------------------
void Put_Value(GIDFILE *f,paramrec *p)
{
  parameter *cur, *prv;
  element *curr, *prev;
  
  if (f->cache == NULL)
  {
    f->cache = new parameter;
    f->cache->next = NULL;
    rstrcpy(f->cache->name,p->name);
    rstrcpy(f->cache->uunit,p->uunit);
    rstrcpy(f->cache->punit,p->punit);
    f->cache->entries = new element;
    f->cache->entries->parent = f->cache;
    f->cache->entries->next = NULL;
    parm2entry(p,f->cache->entries);
  }
  else
  {
    cur = f->cache;
    while (rstrcmpi(cur->name,p->name) && (cur != NULL))
    {
      prv = cur;
      cur = cur->next;
    }
    if (cur == NULL)
    {
      prv->next = new parameter;
      cur = prv->next;
      cur->next = NULL;
      rstrcpy(cur->name,p->name);
      rstrcpy(cur->uunit,p->uunit);
      rstrcpy(cur->punit,p->punit);
      cur->entries = new element;
      cur->entries->parent = cur;
      cur->entries->next = NULL;
      parm2entry(p,cur->entries);
    }
    else
    {
      curr = cur->entries;
      while (!idxequal(p,curr) && curr != NULL)   // check for duplicate entries
      {
        prev = curr;
        curr = curr->next;
      }
      if (curr == NULL)
      {
        prev->next = new element;
        curr = prev->next;
        curr->next = NULL;
        curr->parent = cur;
        curr->next = NULL;
        parm2entry(p,curr);
      }
      else
        printf("Duplicate %s parameter found, using first value!\n",p->name);
    }
  }
}
//------------------------------------------------------------------------------
int Get_Element_Ref(element *e,paramrec *p)
{
  element *curr;
  
  curr = e;
  while (!idxequal(p,curr) && curr != NULL)                // find correct index
    curr = curr->next;
  if (curr != NULL)
    return curr->ref;
  else                                                        // found parameter
    return -1;
}
//------------------------------------------------------------------------------
int Get_Element_Ref(parameter *pm, paramrec *p)
{
  element *curr;
  
  curr = pm->entries;
  while (curr != NULL && !idxequal(p,curr))                // find correct index
    curr = curr->next;
  if (curr != NULL)
    return curr->ref;
  else                                                        // found parameter
    return -1;
}
//------------------------------------------------------------------------------
char *Get_Element_Value(element *e, paramrec *p)
{
  //  begins search with passed in 'element'
  //  return valu by matching indices specified by 'paramrec'
  element *curr;
  
  curr = e;
  while (!idxequal(p,curr) && curr != NULL)                // find correct index
    curr = curr->next;
  if (curr != NULL)
    return curr->valu;                                        // found parameter
  else
    return NULL;
}
//------------------------------------------------------------------------------
char *Get_Element_Value(parameter *pm, paramrec *p)
//  search 'parameter' passed in and back filling 'paramrec'
//  return valu by matching indices specified by 'paramrec'
{
  element *curr;
  
  if (pm == NULL) return NULL;
  curr = pm->entries;
  p->ref = 0;
  rstrcpy(p->punit,"");
  rstrcpy(p->uunit,"");
  rstrcpy(p->valu,"");
  while (curr != NULL && !idxequal(p,curr))                // find correct index
    curr = curr->next;
  if (curr != NULL)
  {
    p->ref = curr->ref;
    rstrcpy(p->punit,pm->punit);
    rstrcpy(p->uunit,pm->uunit);
    rstrcpy(p->valu,curr->valu);
    return p->valu;                                        // found parameter
  }
  return NULL;
}
//------------------------------------------------------------------------------
element *Get_Element(GIDFILE *f,char *pname)
{
  parameter *curr;
  
  curr = f->cache;
  while (rstrcmpi(curr->name,pname) && curr != NULL)      // find parameter name
    curr = curr->next;
  if (curr != NULL)
    return curr->entries;                                 // found element
  else
    return NULL;
}
//------------------------------------------------------------------------------
parameter *Get_Parameter(GIDFILE *f,char *pname)
{
  parameter *curr;
  
  curr = f->cache;
  while (rstrcmpi(curr->name,pname) && curr != NULL)      // find parameter name
    curr = curr->next;
  if (curr != NULL)
    return curr;                                          // found element
  else
    return NULL;
}
//------------------------------------------------------------------------------
char *Get_Value(GIDFILE *f,paramrec *p)
{
  return Get_Element_Value(Get_Parameter(f,p->name),p);
}
//------------------------------------------------------------------------------
int Load_GID(GIDFILE *f,int idx,char *model)
{
  paramrec *pa;
  int i;
  
  if (f != NULL)
  {
    do
    if (readglyph(f))
      if (!rstrcmpi(glyph.name,"fui"))
        for (i=0; i<glyph.cnt; i++)
        {
          pa = readparamrec(f);
          if (pa->cnt1 == idx)
            Put_Value(f,pa);
        }
        else if (!rstrcmpi(glyph.name,"csm"))
          for (i=0; i<glyph.cnt; i++)
          {
            pa = readparamrec(f);
            if (pa->cnt1 == idx)
              Put_Value(f,pa);
          }
          else
            if (!rstrcmpi(glyph.name,model))
              for (i=0; i<glyph.cnt; i++)
                Put_Value(f,readparamrec(f));
              else
                for (i=0; i<glyph.cnt; i++)
                  f->f.readln();
                else
                  return 0;
                while (!f->f.eof());
                return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
int Load_GIDSection(GIDFILE *f,char *model)
{
  int i;
  
  f->f.rewind();
  if (f != NULL)
  {
    do
    if (readglyph(f))
      if (!rstrcmpi(glyph.name,model))
      {
        for (i=0; i<glyph.cnt; i++)
          Put_Value(f,readparamrec(f));
        return 1;
      }
      else
        for (i=0; i<glyph.cnt; i++)
          f->f.readln();
        while (!f->f.eof());
        return 1;
  }
  else
    return 0;
}
//------------------------------------------------------------------------------
GIDFILE *Open_GID(char *s)
{
  GIDFILE *p;
  
  p = new GIDFILE;
  if (p->f.open(s,READ) != 1)
  {
    delete p;
    return NULL;
  }
  else
  {
    p->cache = NULL;
    return p;
  }
}
//------------------------------------------------------------------------------
void Close_GID(GIDFILE *f)
{
  parameter *nxt, *cur;
  element *next, *curr;
  
  if (f!=NULL)
  {
    f->f.close();
    cur = f->cache;
    while (cur != NULL)
    {
      curr = cur->entries;
      while (curr != NULL)
      {
        next = curr->next;
        delete curr;
        curr = next;
      }
      nxt = cur->next;
      delete cur;
      cur = nxt;
    }
    delete f;
  }
}
//------------------------------------------------------------------------------
char *info(GIDFILE *f,char *s,int c1,int c2,int c3,int c4,int c5,int c6)
{
  paramrec pa;
  
  rstrcpy(pa.name,s);
  pa.cnt1 = c1;  pa.cnt2 = c2;
  pa.cnt3 = c3;  pa.cnt4 = c4;
  pa.cnt5 = c5;  pa.cnt6 = c6;
  return Get_Value(f,&pa);
}
//------------------------------------------------------------------------------
int readStr(GIDFILE *f,char *pname,char **val,int idx1,int idx2,int idx3, int idx4, int idx5, int idx6)
{
  char *t;
  *val = NULL;
  t=info(f,pname,idx1,idx2,idx3,idx4,idx5,idx6);
  if (t==NULL) return 0;
  *val = strdup(t);
  return 1;
}
//------------------------------------------------------------------------------
int readFloat(GIDFILE *f,char *pname,float *val,int idx1,int idx2,int idx3, int idx4, int idx5, int idx6)
{
  char *t;
  *val = 0.0;
  t=info(f,pname,idx1,idx2,idx3,idx4,idx5,idx6);
  if (t==NULL) return 0;
  *val = (float)atof(t);
  return 1;
}
//------------------------------------------------------------------------------
int readDouble(GIDFILE *f,char *pname,double *val,int idx1,int idx2,int idx3, int idx4, int idx5, int idx6)
{
  char *t;
  *val = 0.0;
  t=info(f,pname,idx1,idx2,idx3,idx4,idx5,idx6);
  if (t==NULL) return 0;
  *val = atof(t);
  return 1;
}
//------------------------------------------------------------------------------
int readInt(GIDFILE *f,char *pname,int *val,int idx1,int idx2,int idx3, int idx4, int idx5, int idx6)
{
  char *t;
  *val = 0;
  t=info(f,pname,idx1,idx2,idx3,idx4,idx5,idx6);
  if (t==NULL) return 0;
  *val = atoi(t);
  return 1;
}
//------------------------------------------------------------------------------

