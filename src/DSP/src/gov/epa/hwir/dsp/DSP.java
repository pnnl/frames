package gov.epa.hwir.dsp;

import gov.epa.hwir.util.hwirio;
import javax.swing.UIManager;
import javax.swing.*;
import gov.epa.hwir.util.StatDll;

public class DSP {

  public static final boolean DEBUG = false;

  public DSP() {
  }

  public DSP( String site, String regional, String national ) {

    InputFrame inputFrame = new InputFrame( site, regional, national );
    inputFrame.show();

  }

  public static void main(String[] args) {

    try {
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

     //System.exit(0);
      //"-Djava.library.path = gov.epa.hwir.util.javaHWIRIO.Debug.javaHWIRIO"
      //System.out.println( System.getProperty("java.library.path"));
      hwirio hio = new hwirio();


      if (args.length != 3) {

        //Dummy arguments
        hio.setArgs(
            "65536 256 c:\\3mra\\ssf  c:\\3mra\\grf 1 hdprod.ssf hdtest.grf");
        hio.openGroups();

        if (DEBUG){
          System.out.println("Incorrect number of arguments ");
          System.out.println("Usage - DSP <SSF_DIR> <GRF_DIR> <HEADER_FILE>");
        }

        hio.warning("Usage: DSP <SSF_DIR> <GRF_DIR> <HEADER_FILE>");
        hio.error(
            "Improper arguments passed to SDP - read warning file for usage");
        System.exit(1);

      }
      else {
        String ssfDir = args[0];
        String grfDir = args[1];
        String headerFile = args[2];

        hio.setArgs("65536 256 " + ssfDir + " " + grfDir + " 1 " + headerFile +
                    " hdtest.grf");
        hio.openGroups();
        StatDll stat = new StatDll();

        double [] array = new double[] {0.05, 4, 0.04, 0.004, 23, 23.345};

        System.out.println ("Calling a function from statdll ");
        stat.StatSeed( 234 );

        String nationPath = hio.readString("hdprod.ssf", "NationDB", "");
        String sitePath = hio.readString("hdprod.ssf", "SiteBasedDB", "");
        String regionPath = hio.readString("hdprod.ssf", "RegionDB", "");
        String staticNationDB = hio.readString("hdprod.ssf", "StaticNationDB", "");

        DSP dsp = new DSP( sitePath, regionPath, nationPath );

        hio.closeGroups();

        if ( DEBUG ){
          System.out.println( "Calling DSP with 3 arguments " );
          System.out.println( "The three output databases are " ) ;
          System.out.println( "Site: " + sitePath ) ;
          System.out.println( "National: " + nationPath ) ;
          System.out.println( "Regional: " + regionPath ) ;
        }

      }
    }
    catch (Exception e) {
      e.printStackTrace();
    }

  }
}
