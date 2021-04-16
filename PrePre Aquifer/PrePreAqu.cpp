#pragma hdrstop
#include <condefs.h>
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\Line.cpp");
USEUNIT("..\Common Files\PolyClass.cpp");
USEUNIT("gid.cpp");
USEUNIT("types.cpp");
USEUNIT("offset.cpp");
USEUNIT("GFFReader.cpp");
USEUNIT("WFFReader.cpp");
USERES("PrePreAqu.res");
//---------------------------------------------------------------------------
#include "gid.h"
#include "WFFReader.h"
#include "GFFReader.h"
#include "PolyClass.h"
#include "offset.h"
#include <iostream.h>
//------------------------------------------------------------------------------
#pragma argsused
icsv *in;
ocsv *out;
ocsv *gidout;
Section *gis;
GIDfile *gid;
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
int CheckParameters(int argcount)
{
        char answer[5];
        if(argcount != 6)
        {
                cout << "Not enough parameters passed to program" << endl;
                cout << "Contact PNNL" << endl;
                cout << "Press any key followed by ENTER" << endl;
                scanf("%s",answer);
                return 0;
        }
        return 1;
}
//------------------------------------------------------------------------------
int WriteError(FILE *fp,char *errmessage,char *ext,char *path)
{
        char errmsg[1024];
        sprintf(errmsg,"%s %s.%s",errmessage,path,ext);
        fwrite(errmsg,strlen(errmsg),1,fp);
        fclose(fp);
        return 0;
}
//------------------------------------------------------------------------------
int PointInsidePoly(PointClass* pnt,PolygonClass* poly)
{
    int i, j, c = 0;
    for (i = 0, j = poly->size-1; i < poly->size; j = i++)
    {
        if ((((poly->points[i]->y<=pnt->y) && (pnt->y<poly->points[j]->y)) ||((poly->points[j]->y<=pnt->y) && (pnt->y<poly->points[i]->y))) &&
            (pnt->x <= (poly->points[j]->x - poly->points[i]->x) * (pnt->y - poly->points[i]->y) / (poly->points[j]->y - poly->points[i]->y) + poly->points[i]->x))
                c = !c;
    }
    return c;
}
//------------------------------------------------------------------------------
int getoffset(Segment** segs,Point *pt,double *dist,double *offset)
{
        *dist = 5.987;
        *offset = 6.543;
        return 1;
}
//------------------------------------------------------------------------------
void setindices(long *idxs,long idx1=0,long idx2=0,long idx3=0,long idx4=0,long idx5=0,long idx6=0)
{
        idxs[0] = idx1;
        idxs[1] = idx2;
        idxs[2] = idx3;
        idxs[3] = idx4;
        idxs[4] = idx5;
        idxs[5] = idx6;
}
//------------------------------------------------------------------------------
int main(int argc, char* argv[])
{
        char answer[5];
        char errfilepath[256];
        char gidpath[256];
        char errmsg[256];
        char srcname[256];
        char gisname[256];
        char objlabel[256];
        char objtype[256];
        double tempx,tempy,polywidth;
        double *xcoords;
        double *ycoords;
        double *zcoords;
        int numpts;
        double *distxcoords;
        double *distycoords;
        double *distances;
        double *offsets;
        double *ldisp;
        double *tdisp;
        double *vdisp;
        double wzcdist;
        double wzcydist;
        double wzcaqdepth;
        double wzcldisp;
        double wzctdisp;
        double wzcvdisp;
        int nmods,index,numaquobjs,aqumodindex,polyindex,stlineindex,gridindex;
        int check,i,numsegs;
        int *aquobjindex;
        long idxs[6];
        bool aqufound;
        bool fuifound;
        int INTERSECT;
        Segment         *streampoly;
        Section         **tsection;
        Section         *aqusection;
        Section         *fuisection;
        Section         *gissection;
        PointClass      *streamline;
        PolygonClass    *plumepolys;
        GFFfile         *gfffile;
        WFFfile         *wfffile;
        GFFMod          *vadmod;
        PolygonClass    *vadpoly;
        PolygonClass    *aqupoly;
        Parameter       *tparm;
        Entry           *tentry;
        GIDfile         *gfile;
        FILE            *errfp;
        Point           *gridpoint;
        char* distunit;
        char* ydistunit;
        char* dpunit;
        char* ldispunit;
        char* tdispunit;
        char* vdispunit;

        vadpoly = NULL;
        ldisp = NULL;
        tdisp = NULL;
        vdisp = NULL;
        distxcoords = NULL;
        distycoords = NULL;
        zcoords = NULL;
        if(!CheckParameters(argc)) return 0;
        sprintf(errfilepath,"%s.err",argv[2]);
        errfp = fopen(errfilepath,"w");
        if(!errfp)
        {
                cout << "Unable to open error file" << endl;
                cout << "Program terminating" << endl;
                cout << "Press any key followed by ENTER" << endl;
                scanf("%s",answer);
                return 0;
        }
        sprintf(gidpath,"%s.gid",argv[1]);
        gfile = new GIDfile(gidpath);
        check = gfile->Read();
        if(check == 0)return WriteError(errfp,"Unable to read GID file:","gid",argv[1]);
        tsection = new Section*[gfile->numsects];
        for(i=0;i<gfile->numsects;i++) tsection[i] = gfile->GetSection(i);
        aqufound = false;
        fuifound = false;
        for(i=0;i<gfile->numsects;i++)
        {
                if(strcmpi(tsection[i]->name,argv[5])==0)
                {
                        aqusection = tsection[i];
                        aqufound = true;
                }
                if(strcmpi(tsection[i]->name,"FUI")==0)
                {
                        fuisection = tsection[i];
                        fuifound = true;
                }
                if(aqufound && fuifound)
                        break;
        }
        if(aqufound == false)return WriteError(errfp,"Aquifer Section not found in GID file:","gid",argv[1]);
        if(fuifound == false)return WriteError(errfp,"FUI Section not found in GID file:","gid",argv[1]);
        tparm = fuisection->GetParameter("AquSrcName");
        setindices(idxs,atoi(argv[3]),atoi(argv[4]),1);
        tentry = tparm->GetEntry(idxs);
        rstrcpy(srcname,tentry->value);

        tparm = fuisection->GetParameter("AquType");
        setindices(idxs,atoi(argv[3]),atoi(argv[4]),1);
        tentry = tparm->GetEntry(idxs);
        if (strcmpi(tentry->value,"Aquifer") != 0)
        {
          gfffile = new GFFfile();
          check = gfffile->Read(argv[1]);
          if(check == 0)return WriteError(errfp,"Unable to read GFF file:","gff",argv[1]);
          wfffile = new WFFfile();
          check = wfffile->Read(argv[1]);
          if(check)
          {
                  for(i=0;i<wfffile->nummod;i++)
                  {
                          if(strcmpi(srcname,wfffile->module[i]->name)==0)
                          {
                                  polywidth = wfffile->module[i]->moddata[0]->width;
                          }
                  }
          }
          //if(check == 0)return WriteError(errfp,"Unable to read WFF file:","wff",argv[1]);
          if(check)check = wfffile->WriteExcept(argv[1],srcname);
  //        if(check == 0)return WriteError(errfp,"Unable to write WFF file:","wff",argv[1]);
          check = gfffile->WriteWFFSection(argv[1],srcname);
          if(check == 0)return WriteError(errfp,"Unable to write WFF file:","wff",argv[1]);
          for(i=0;i<gfffile->nummod;i++)
          {
                  if(strcmpi(gfffile->module[i]->name,srcname)==0)
                  {
                          vadmod = gfffile->module[i];
                          break;
                  }
          }
          vadpoly = new PolygonClass();
          for(i=0;i<vadmod->moddata[0]->numvertices;i++)
                  vadpoly->addPoint(vadmod->moddata[0]->x[i],vadmod->moddata[0]->y[i]);
          INTERSECT = 1;
        }
        else
        {
          gfffile = NULL;
          vadpoly = NULL;
//          wfffile = NULL;
          wfffile = new WFFfile();
          check = wfffile->Read(argv[1]);
          if(check)
          {
                  for(i=0;i<wfffile->nummod;i++)
                  {
                          if(strcmpi(srcname,wfffile->module[i]->name)==0)
                          {
                                  polywidth = wfffile->module[i]->moddata[0]->width;
                          }
                  }
          }
          INTERSECT = 0;
        }

        tparm = fuisection->GetParameter("GISName");
        setindices(idxs,1,1);
        tentry = tparm->GetEntry(idxs);
        rstrcpy(gisname,tentry->value);
        for(i=0;i<gfile->numsects;i++)
        {
                if(strcmpi(tsection[i]->name,gisname)==0)
                {
                        gissection = tsection[i];
                        break;
                }
        }

        tparm = gissection->GetParameter("NumModules");
        setindices(idxs);
        tentry = tparm->GetEntry(idxs);
        nmods = atoi(tentry->value);
        tparm = gissection->GetParameter("ModuleId");
        for(i=0;i<nmods;i++)
        {
                setindices(idxs,i+1);
                tentry = tparm->GetEntry(idxs);
                if(strcmpi(tentry->value,argv[5])==0)
                {
                        aqumodindex = tentry->idx[0];
                        break;
                }
        }
        tparm = gissection->GetParameter("NumModGeoObj");
        setindices(idxs,aqumodindex);
        tentry = tparm->GetEntry(idxs);
        numaquobjs = atoi(tentry->value);
        aquobjindex = new int[numaquobjs];
        tparm = gissection->GetParameter("ModGeoObjIndex");
        for(i=0;i<numaquobjs;i++)
        {
                setindices(idxs,aqumodindex,i+1);
                tentry = tparm->GetEntry(idxs);
                aquobjindex[i] = atoi(tentry->value);
        }
        polyindex = -99;
        gridindex = -99;
        stlineindex = -99;
        for(i=0;i<numaquobjs;i++)
        {
                setindices(idxs,aquobjindex[i]);
                tparm = gissection->GetParameter("GeoObjLabel");
                tentry = tparm->GetEntry(idxs);
                rstrcpy(objlabel,tentry->value);
                tparm = gissection->GetParameter("GeoObjType");
                tentry = tparm->GetEntry(idxs);
                rstrcpy(objtype,tentry->value);
                tparm = gissection->GetParameter("GeoObjIndex");
                tentry = tparm->GetEntry(idxs);
                if(strcmpi(objlabel,"boundary")==0 && strcmpi(objtype,"polygon")==0)
                        polyindex = atoi(tentry->value);
                else if(strcmpi(objlabel,"grid")==0 && strcmpi(objtype,"pointgrid")==0)
                        gridindex = atoi(tentry->value);
                else if(strcmpi(objlabel,"stline")==0 && strcmpi(objtype,"polyline")==0)
                        stlineindex = atoi(tentry->value);
        }
        if(polyindex == -99)return WriteError(errfp,"Unable to read Aquifer Polygon","","");
        if(gridindex == -99)return WriteError(errfp,"Unable to read Aquifer Grid","","");
        if(stlineindex == -99)return WriteError(errfp,"Unable to read Aquifer Streamline","","");
        aqupoly = new PolygonClass();
        tparm = gissection->GetParameter("NumPolygonPts");
        setindices(idxs,polyindex);
        tentry = tparm->GetEntry(idxs);
        index = atoi(tentry->value);
        tparm = gissection->GetParameter("PolygonPts");
        for(i=0;i<index;i++)
        {
                setindices(idxs,polyindex,i+1,1);
                tentry = tparm->GetEntry(idxs);
                tempx = atof(tentry->value);
                idxs[2] = 2;
                tentry = tparm->GetEntry(idxs);
                tempy = atof(tentry->value);
                aqupoly->addPoint(tempx,tempy);
        }
        tparm = gissection->GetParameter("NumPolylinePts");
        setindices(idxs,stlineindex);
        tentry = tparm->GetEntry(idxs);
        index = atoi(tentry->value);
        streamline = new PointClass[index];
        tparm = gissection->GetParameter("PolylinePts");
        for(i=0;i<index;i++)
        {
                setindices(idxs,stlineindex,i+1,1);
                tentry = tparm->GetEntry(idxs);
                tempx = atof(tentry->value);
                idxs[2] = 2;
                tentry = tparm->GetEntry(idxs);
                tempy = atof(tentry->value);
//                streamline[i] = new PointClass();
                streamline[i].x = tempx;
                streamline[i].y = tempy;
        }
        if (INTERSECT)
        {
          check = PointInsidePoly(&streamline[0],vadpoly);
          if(!check)return WriteError(errfp,"Starting point of streamline does not reside within the Vadose boundary polygon","","");
          for(i=0;i<index;i++)
          {
                  check = PointInsidePoly(&streamline[i],aqupoly);
                  if(!check)break;
          }
          if(!check)return WriteError(errfp,"All points of streamline do not reside within the Aquifer boundary polygon","","");
        }
        else
        {

        }

        streampoly = new Segment[index-1];
        numsegs = index -1;
        for(i=0;i<numsegs;i++)
                streampoly[i].segmentinit(streamline[i].x,streamline[i].y,streamline[i+1].x,streamline[i+1].y);

        plumepolys = CreatePolyPlume(streampoly,numsegs,polywidth);

        tparm = gissection->GetParameter("NumGridPts");
        setindices(idxs,gridindex);
        tentry = tparm->GetEntry(idxs);
        numpts = atoi(tentry->value);
        xcoords = new double[numpts];
        ycoords = new double[numpts];
        zcoords = new double[numpts];
        for(i=1;i<=numpts;i++)
        {
                tparm = gissection->GetParameter("GridPtCoord");
                setindices(idxs,gridindex,i,1);
                tentry = tparm->GetEntry(idxs);
                xcoords[i-1] = atof(tentry->value);
                setindices(idxs,gridindex,i,2);
                tentry = tparm->GetEntry(idxs);
                ycoords[i-1] = atof(tentry->value);
                setindices(idxs,gridindex,i,3);
                tentry = tparm->GetEntry(idxs);
                zcoords[i-1] = atof(tentry->value);
        }


/*
        tparm = gissection->GetParameter("NumCartXDist");
        setindices(idxs,gridindex);
        tentry = tparm->GetEntry(idxs);
        numxdist = atoi(tentry->value);
        tparm = gissection->GetParameter("NumCartYDist");
        setindices(idxs,gridindex);
        tentry = tparm->GetEntry(idxs);
        numydist = atoi(tentry->value);
        tparm = gissection->GetParameter("CartLLCoord");
        setindices(idxs,gridindex,1);
        tentry = tparm->GetEntry(idxs);
        llxcoord = atof(tentry->value);
        idxs[1] = 2;
        tentry = tparm->GetEntry(idxs);
        llycoord = atof(tentry->value);
        tparm = gissection->GetParameter("CartURCoord");
        setindices(idxs,gridindex,1);
        tentry = tparm->GetEntry(idxs);
        urxcoord = atof(tentry->value);
        idxs[1] = 2;
        tentry = tparm->GetEntry(idxs);
        urycoord = atof(tentry->value);
        gridxdiv = (urxcoord - llxcoord) / (numxdist-1);
        gridydiv = (urycoord - llycoord) / (numydist-1);
        xcoords = new double[numxdist];
        ycoords = new double[numydist];
*/




//        for(i=0;i<numxdist;i++)
//                xcoords[i] = llxcoord + (i * gridxdiv);
//        for(i=0;i<numydist;i++)
//                ycoords[i] = llycoord + (i * gridydiv);
//        check = aqusection->DelParameter("wzcname");
//        check = aqusection->DelParameter("wzcdist");
//        check = aqusection->DelParameter("wzcydist");
//        check = aqusection->DelParameter("wzcaqdepth");
//        check = aqusection->DelParameter("wzcldisp");
//        check = aqusection->DelParameter("wzctdisp");
//        check = aqusection->DelParameter("wzcvdisp");
//        check = aqusection->DelParameter("wzcnum");


//        distances = new double[numxdist * numydist];
//        offsets = new double[numxdist * numydist];
        distances = new double[numpts];
        offsets = new double[numpts];



//        ldisp = new double[numxdist * numydist];
//        tdisp = new double[numxdist * numydist];
//        vdisp = new double[numxdist * numydist];


//        distxcoords = new double[numxdist * numydist];
//        distycoords = new double[numxdist * numydist];
        distxcoords = new double[numpts];
        distycoords = new double[numpts];

        check = gfile->WriteExcept(aqusection->name);
        gidout = new ocsv(gidpath,'"',',',_APPEND_);
        gridpoint = new Point();
        index = 0;
        for(i=0;i<numpts;i++)
        {
                gridpoint->x = xcoords[i];
                gridpoint->y = ycoords[i];
                check = getoffset(plumepolys,streampoly,numsegs,gridpoint,&distances[index],&offsets[index]);
                if(check)
                {
                        distxcoords[index] = xcoords[i];
                        distycoords[index] = ycoords[i];
                        index++;
                }
        }


/*
        for(i=0;i<numxdist;i++)
        {
                gridpoint->x = xcoords[i];
                for(j=0;j<numydist;j++)
                {
                        setindices(idxs,j);
                        gridpoint->y = ycoords[j];
                        check = getoffset(plumepolys,streampoly,numsegs,gridpoint,&distances[index],&offsets[index]);
                        if(check)
                        {
                                distxcoords[index] = xcoords[i];
                                distycoords[index] = ycoords[i];
                                ldisp[index] = 0.1 * distances[index];
                                tdisp[index] = 0.33 * ldisp[index];
                                vdisp[index] = 0.0025 * ldisp[index];
                                index++;
                        }
                }
        }
*/

        tparm = aqusection->GetParameter("wzcname");
        setindices(idxs,0);
        tentry = tparm->GetEntry(idxs);
        if(strcmpi(tentry->value,"grid")!=0) WriteError(errfp,"Unable to retrieve grid data from aquifer section","","");

        tparm = aqusection->GetParameter("wzcdist");
        tentry = tparm->GetEntry(idxs);
        wzcdist = atof(tentry->value);
        distunit = strdup(tentry->usrunit);

        tparm = aqusection->GetParameter("wzcydist");
        tentry = tparm->GetEntry(idxs);
        wzcydist = atof(tentry->value);
        ydistunit = strdup(tentry->usrunit);

        tparm = aqusection->GetParameter("wzcaqdepth");
        tentry = tparm->GetEntry(idxs);
        wzcaqdepth = atof(tentry->value);
        dpunit = strdup(tentry->usrunit);
        tparm = aqusection->GetParameter("wzcldisp");
        tentry = tparm->GetEntry(idxs);
        wzcldisp = atof(tentry->value);
        ldispunit = strdup(tentry->usrunit);
        tparm = aqusection->GetParameter("wzctdisp");
        tentry = tparm->GetEntry(idxs);
        wzctdisp = atof(tentry->value);
        tdispunit = strdup(tentry->usrunit);
        tparm = aqusection->GetParameter("wzcvdisp");
        tentry = tparm->GetEntry(idxs);
        wzcvdisp = atof(tentry->value);
        vdispunit = strdup(tentry->usrunit);
        aqusection->count = aqusection->GetCount();
        check = aqusection->DelParameter("wzcname");
        check = aqusection->DelParameter("wzcdist");
        check = aqusection->DelParameter("wzcydist");
        check = aqusection->DelParameter("wzcaqdepth");
        check = aqusection->DelParameter("wzcldisp");
        check = aqusection->DelParameter("wzctdisp");
        check = aqusection->DelParameter("wzcvdisp");
        check = aqusection->DelParameter("wzcnum");
        aqusection->count = aqusection->GetCount();
        aqusection->count = aqusection->GetCount() + ((index*7)+8);
        check = aqusection->Write(gidout);
        *gidout << "wzcname" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << "N/A" << "N/A" << "grid" << NewLn;
        *gidout << "wzcdist" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << distunit << "cm" << wzcdist << NewLn;
        *gidout << "wzcydist" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << ydistunit << "cm" << wzcydist << NewLn;
        *gidout << "wzcaqdepth" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << dpunit << "cm" << wzcaqdepth << NewLn;
        *gidout << "wzcldisp" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << ldispunit << "cm" << wzcldisp << NewLn;
        *gidout << "wzctdisp" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << tdispunit << "cm" << wzctdisp << NewLn;
        *gidout << "wzcvdisp" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << vdispunit << "cm" << wzcvdisp << NewLn;
        *gidout << "wzcnum" << 0 << 0 << 0 << 0 << 0 << 0 << 0 << "N/A" << "N/A" << index << NewLn;
        for(i=0;i<index;i++)
        {
                sprintf(errmsg,"%d:%d",(int)distxcoords[i],(int)distycoords[i]);
                *gidout << "wzcname" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << "N/A" << "N/A" << errmsg << NewLn;
                *gidout << "wzcdist" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << "m" << "cm" << 100*distances[i] << NewLn;
                *gidout << "wzcydist" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << "m" << "cm" << 100*offsets[i] << NewLn;
                *gidout << "wzcaqdepth" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << dpunit << "cm" << wzcaqdepth << NewLn;
                *gidout << "wzcldisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << ldispunit << "cm" << wzcldisp << NewLn;
                *gidout << "wzctdisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << tdispunit << "cm" << wzctdisp << NewLn;
                *gidout << "wzcvdisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << -1 << vdispunit << "cm" << wzcvdisp << NewLn;
//                *gidout << "wzcaqdepth" << (i+1) << 0 << 0 << 0 << 0 << 0 << 0 << "m" << "m" << 0.0 << NewLn;
//                *gidout << "wzcldisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << 0 << "m" << "m" << ldisp[i] << NewLn;
//                *gidout << "wzctdisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << 0 << "m" << "m" << tdisp[i] << NewLn;
//                *gidout << "wzcvdisp" << (i+1) << 0 << 0 << 0 << 0 << 0 << 0 << "m" << "m" << vdisp[i] << NewLn;
        }
        delete[] distunit;
        delete[] ydistunit;
        delete[] dpunit;
        delete[] ldispunit;
        delete[] tdispunit;
        delete[] vdispunit;
        delete gidout;
        if(xcoords)
                delete[] xcoords;
        if(ycoords)
                delete[] ycoords;
        if(zcoords)
                delete[] zcoords;
        if(distxcoords)
                delete[] distxcoords;
        if(distycoords)
                delete[] distycoords;
        if(plumepolys)
                delete[] plumepolys;
        if(ldisp)
                delete[] ldisp;
        if(tdisp)
                delete[] tdisp;
        if(vdisp)
                delete[] vdisp;
        if(gridpoint)
                delete gridpoint;
        if(gfile)
                delete gfile;
        if(vadpoly)
                delete vadpoly;
        if(aqupoly)
                delete aqupoly;
        if(gfffile)
                delete gfffile;
        if(wfffile)
                delete wfffile;
        if(streamline)
                delete[] streamline;
        if(streampoly)
                delete[] streampoly;
        if(distances)
                delete[] distances;
        if(offsets)
                delete[] offsets;
        fclose(errfp);
        unlink(errfilepath);
        return 0;
}
//---------------------------------------------------------------------------

