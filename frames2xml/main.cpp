#include "Frames2XML.h"
#include "..\Common Files\ReferencesC.h"
#include <vector>
using namespace std;

#define PARSE_SIM 0
#define PARSE_MOD 1
#define PARSE_UIIO 2
#define PARSE_METADATA 3
#define PARSE_SCHEME 4
#define PARSE_INPUT 5
#define PARSE_OUTPUT 6
#define PARSE_REFS 7
#define PARSE_ARGUMENTS 8

void outputArguments()
{
    cout << "-xml file   -specifies the file where output should" << endl
         << "             be stored" << endl;
    cout << "-xsl file   -specifieds the xsl file that will be" << endl
         << "             used to parse this xml file for display" << endl;
}

int main(int argc, char **argv)
{
    string fileXML = "fdr.xml";
    string fileXSL = "fdr.xsl";
    string optionXML = "-xml";
    string optionXSL = "-xsl";
    string optionParse = "-parse";

    vector<int> listParse;

    for(int i = 0; i < argc; i++)
    {
        if(!stricmp(optionXML.c_str(), argv[i]))
        {
            if(argc > i + 1)
                fileXML = argv[i+1];
        }
        else if (!stricmp(optionXSL.c_str(), argv[i]))
        {
            if(argc > i + 1)
                fileXSL = argv[i+1];
        }
        else if (!stricmp(optionParse.c_str(), argv[i]))
        {
            if(argc > i + 1)
            {
              if (!stricmp(argv[i+1], "sim"))
                listParse.push_back(PARSE_SIM);
              else if (!stricmp(argv[i+1], "mod"))
                listParse.push_back(PARSE_MOD);
              else if (!stricmp(argv[i+1], "uiio"))
                listParse.push_back(PARSE_UIIO);
              else if (!stricmp(argv[i+1], "metadata"))
                listParse.push_back(PARSE_METADATA);
              else if (!stricmp(argv[i+1], "scheme"))
                listParse.push_back(PARSE_SCHEME);
              else if (!stricmp(argv[i+1], "input"))
                listParse.push_back(PARSE_INPUT);
              else if (!stricmp(argv[i+1], "output"))
                listParse.push_back(PARSE_OUTPUT);
              else if (!stricmp(argv[i+1], "refs"))
                listParse.push_back(PARSE_REFS);
              else if (!stricmp(argv[i+1], "arguments"))
                listParse.push_back(PARSE_ARGUMENTS);
            }
        }
        else if (!stricmp(argv[i], "--help") || ! stricmp(argv[i], "-help")
                 || ! stricmp(argv[i], "/?"))
        {
            outputArguments();
            return 0;
        }
    }

    ofstream fout(fileXML.c_str());
	///open up frames and output entire simulation to xml
	Frames2XML frames(&fout, fileXSL.c_str());
	if(frames.openSystem(argv, argc))
	{
//AddVarRef(frames.getPID(), "SUExample.sim.Mod1.SimpleUI", "C", 2,0,0,0,0,0,"ACGIH");
//AddVarRef(frames.getPID(), "SUExample.sim.Mod1.SimpleUI", "A", 0,0,0,0,0,0,"testing");
	    ///currently only outputs date and time
	    frames.outputHeader();

      ///parse the simulation by default if no parse options are specified.
      if(listParse.size() == 0)
        listParse.push_back(PARSE_SIM);

      char *mid = frames.passedModuleName();
      char mname[4096];
      strcpy(mname, "");

      bool moduleHeaderOpen = false;
      for(int i = 0; i < listParse.size();i++)
      {
        bool moduleProperty = listParse[i] >= PARSE_UIIO && listParse[i] <= PARSE_OUTPUT;
        if(moduleProperty && !moduleHeaderOpen)
        {
          frames.outputModuleHeader(frames.getIndexFromId(mid));
          moduleHeaderOpen = true;
        }
        else if(!moduleProperty && moduleHeaderOpen)
        {
          frames.outputModuleFooter();
          moduleHeaderOpen = false;
        }

        switch(listParse[i])
        {
        case PARSE_SIM:
    	    if(!frames.outputSimulation())
            frames.outputMessage("error", "Error outputing simulation");
          break;
        case PARSE_MOD:
    	    if(!frames.outputModule(mid))
            frames.outputMessage("error", "Error outputting module");
          break;
        case PARSE_UIIO:
    	    if(!frames.outputModuleUIIO(mid))
            frames.outputMessage("error", "Error outputing module UIIO");
          break;
        case PARSE_METADATA:
          frames.getNameFromId(mid, mname);
          if(!frames.outputModuleMetadata(mname))
            frames.outputMessage("error", "Error outputing module metadata");
          break;
        case PARSE_SCHEME:
    	    if(!frames.outputModuleScheme(mid))
            frames.outputMessage("error", "Error outputing module scheme");
          break;
        case PARSE_INPUT:
    	    if(!frames.outputModuleIO(mid, true))
            frames.outputMessage("error", "Error outputing module inputs");
          break;
        case PARSE_OUTPUT:
    	    if(!frames.outputModuleIO(mid, false))
            frames.outputMessage("error", "Error outputing module outputs");
        break;
        case PARSE_REFS:
    	    frames.outputReferences();
          break;
        case PARSE_ARGUMENTS:
          frames.outputArguments();
          break;
        }
//        if(listParse[i] >= PARSE_UIIO && listParse[i] <= PARSE_OUTPUT)
//          frames.outputModuleFooter();
      }
      ///close the xml module block if it's still open
      if(moduleHeaderOpen)
        frames.outputModuleFooter();
	}
  else
    frames.outputArguments();

  frames.closeTag();

	fout.flush();
	fout.close();

  return 0;
}
