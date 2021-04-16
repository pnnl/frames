//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

USEFORM("ATOConvertUI.cpp", Form1);
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("cnvrect2.cpp");
USERES("ATOConvert.res");

//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
  try
  {
     Application->Initialize();
     Application->CreateForm(__classid(TForm1), &Form1);
        Application->Run();
  }
  catch (Exception &exception)
  {
     Application->ShowException(&exception);
  }
  return 0;
}
//---------------------------------------------------------------------------
