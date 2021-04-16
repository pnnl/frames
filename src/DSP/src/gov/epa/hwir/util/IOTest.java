package gov.epa.hwir.util;

public class IOTest {

  public IOTest() {
  }

  public static void main(String args[])
  {
    hwirio hio = new hwirio();
    hio.setArgs("65536 256 c:\\3mra\\ssf  c:\\3mra\\grf 1 hdprod.ssf hdtest.grf");
    System.out.println("Before OpenGroups");
    hio.openGroups();
    System.out.println("After OpenGroups");
    System.out.println("NumArgs[7]:"+hio.numArgs());
    System.out.println("GetArgInt(1)[65536]:"+hio.getArgInt(1));
    System.out.println("GetArgInt(2)[256]:"+hio.getArgInt(2));
    System.out.println("GetArgString(7)[hdtest.grf]:"+hio.getArgString(7));
    System.out.println("Warning \"this is a test\"");
    hio.warning("This is a test");
    System.out.println("ReadInt(\"hdprod.ssf\",\"ChemCnt\",\"\")[1]:"+hio.readInt("hdprod.ssf","ChemCnt",""));
    System.out.println("ReadInt(\"hdprod.ssf\",\"SiteCnt\",\"\")[201]:"+hio.readInt("hdprod.ssf","SiteCnt",""));
    System.out.println(hio.readString("hdprod.ssf","NationDB",""));

    System.out.println("WriteLog(\"hdtest.grf\",\"Log2\",\"\",1,1,true)[\"T\" in hdtest.grf]:");

    System.out.print("ReadLog(\"hdtest.grf\",\"Log2\",\"\",1,1)[T]:");

    System.out.println("WriteString(\"hdtest.grf\",\"String3\",\"\",1,1,1,\"Test\")[Test in hdtest.grf]:");

    System.out.print("ReadString(\"hdtest.grf\",\"String3\",\"\",1,1,1)[Test]:");
    hio.closeGroups();
    System.out.println("Closed Groups successfully");
  }
}
