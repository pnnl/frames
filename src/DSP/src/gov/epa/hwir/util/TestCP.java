package gov.epa.hwir.util;

import gov.epa.hwir.util.hwircp;
import gov.epa.hwir.util.hwirio;

public class TestCP {
  public TestCP(){
  }
  public static void main(String[] args) {
    int i,n;
    hwircp hcp=new hwircp();
    hcp.setPath("c:\\3MRA\\cppdata");
    System.out.print("Number of chemicals in database:");
    System.out.println(hcp.getNumChem());
    n=hcp.getNumChem();
    for (i=0;i<n;i++)
    {
      System.out.print(hcp.getName(i));
      System.out.print("(");
      System.out.print(hcp.getCASID(i));
      System.out.print(")");
      System.out.println(hcp.getType(i));
    }
    hcp.setDebug(true);
    hcp.setEnvironment(25.0,7.0,hwircp.MEDIA_SOIL,0.20);
    hcp.setCASID("71-43-2");
    System.out.print("Number of solid waste CWs for Benzene:");
    System.out.println(hcp.getNumSolCW());
    n=hcp.getNumSolCW();
    for (i=0;i<n;i++)
    {
      if (i>0) System.out.print(",");
      System.out.print(hcp.getSolCW(i));
    }
    System.out.println();
    System.out.print("Number of liquid waste CWs for Benzene:");
    System.out.println(hcp.getNumLiqCW());
    for (i=0;i<n;i++)
    {
      if (i>0) System.out.print(",");
      System.out.print(hcp.getLiqCW(i));
    }
    System.out.println();
    hwirio hio=new hwirio();
    hio.setArgs("65536 256 d:\\hwir\\ssf d:\\hwir\\grf 1 hdCPTest.grf");
    System.out.println("Before OpenGroups");
    hio.openGroups();
    System.out.println("After OpenGroups");
    hio.addGroup("cptest.ssf");
    hcp.generateSSF("cptest.ssf");
    hio.closeGroups();
    System.out.println("Test Complete");
  }
}
