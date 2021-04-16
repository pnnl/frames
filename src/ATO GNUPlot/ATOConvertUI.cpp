//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ATOConvertUI.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
  : TForm(Owner)
{
  mem.Off();
  cd = new ConvertData(ParamStr(1).c_str(),ParamStr(5).c_str());
  Edit1->Text = cd->CenterY;
  Edit2->Text = cd->CenterX;
  Edit3->Text = cd->TimeSpan;
  Edit4->Text = cd->TimeInterval;
  mem.Newed(cd);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::UpdateGauges(int value,char *Title)
{
  ProgressBar1->Position = value;
  Label5->Caption = (AnsiString)Title + "  " + value + "% complete";
  Label5->Refresh();
}

void __fastcall TForm1::writeDummy(char *fname)
{
  ofstream *s;
  char filename[80];
  sprintf(filename,"%s.csv",fname);
  s=new ofstream(filename); mem.Newed(s);
  *s << "This one file has been saved as two files\n"
     << "deposit.csv and airconc.csv \n";
  s->close();
  delete s;
}

void __fastcall TForm1::Button1Click(TObject *Sender)
{
  ATO *ato;
  Grid *grid;
  ATOEntry *ae,*averaged;
  char title[100];


  cd->CenterY = Edit1->Text.ToDouble();
  cd->CenterX = Edit2->Text.ToDouble();
  cd->TimeSpan = Edit3->Text.ToDouble();
  cd->TimeInterval = Edit4->Text.ToDouble();

// read GID entries for this module
  ato=new ATO(ParamStr(1).c_str(),cd->Source); mem.Newed(ato);
// open and synchronize the ATO file
  grid=new Grid(ParamStr(2).c_str(),cd->CenterX,cd->CenterY,ato->GridType()); mem.Newed(grid);
  ae=new ATOEntry(); mem.Newed(ae);
  averaged=new ATOEntry(); mem.Newed(averaged);
  while (ato->Read(*ae))
  {
    sprintf(title,"%s",ato->PConName);
    UpdateGauges(25,title);
//    ae->IntegrateDeposition();
    averaged->Average(cd->TimeSpan,cd->TimeInterval,ae);
    UpdateGauges(50,title);
    grid->Write(*averaged);
    grid->Write(*ae);
    UpdateGauges(75,title);
    grid->WriteGNU(*averaged);
    grid->WriteGNU(*ae);
    UpdateGauges(100,title);
  }
  mem.Deleting(averaged); delete averaged;
  mem.Deleting(ae); delete ae;
  mem.Deleting(grid); delete grid;
  mem.Deleting(ato); delete ato;
  mem.Deleting(cd); delete cd;
  mem.Dump("End Of Program");
  writeDummy(ParamStr(2).c_str());
  Application->Terminate();
}
//---------------------------------------------------------------------------

void __fastcall TForm1::Button2Click(TObject *Sender)
{
  Application->Terminate();
}
//---------------------------------------------------------------------------

