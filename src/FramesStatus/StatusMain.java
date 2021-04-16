package FramesStatus;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;

import java.sql.*;

/**
 * @author d3k076
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class StatusMain extends HttpServlet {

  /** Initializes the servlet.
     */
    public void init(ServletConfig config) throws ServletException {
        super.init(config);

    }

    /** Destroys the servlet.
     */
    public void destroy() {

    }

    /** Processes requests for both HTTP <code>GET</code> and <code>POST</code> methods.
     * @param request servlet request
     * @param response servlet response
     */
    protected void processRequest(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, java.io.IOException {

    response.setContentType("text/html");
        java.io.PrintWriter out = response.getWriter();

        try {

          // first get a connection to the status database
          DBUtils dbUtils = new DBUtils();
          Connection con = dbUtils.getLocalConnection();

          // now create a statement to extract the data for the table
          Statement st = con.createStatement();
          String strSQL = "SELECT " +
                "t.Unique_ID, " +
                "t.ID, " +
                "t.Charge_Code, " +
                "t.Status_Id, " +
                "t.Name, " +
                "t.Task_Lead, " +
                "(SELECT Max(w.Report_Date) FROM WeeklyStatus w WHERE t.Unique_ID = w.Task_ID) as Report_Date " +
              "FROM " +
                "Tasks t " +
              "WHERE " +
                "t.Summary = 'No' " +
              "ORDER BY " +
                "t.Status_Id";
          // execute the SQL statemnent and return the results
          ResultSet rsTasks = st.executeQuery(strSQL);

          // now print out the html page
          out.println("<html>");
          out.println("<head>");
          out.println("<title>Frames Status Reporting</title>");
          out.println("<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>");
          out.println("<!--");
          out.println("");
          out.println("function button_onclick(uid) {");
      //out.println(" window.alert(\"This is a test \" + uid);");
      out.println(" window.document.statusmain.id.value = uid;");
      //out.println(" window.alert(\"Hidden value is: \" + window.document.statusmain.id.value);");
      out.println("   window.document.statusmain.method = \"POST\";");
      out.println("   window.document.statusmain.action = \"TaskDetail\";");
      out.println("   window.document.statusmain.submit();");
          out.println("}");
          out.println("");
          out.println("//-->");
          out.println("</SCRIPT>");
          out.println("</head>");
          out.println("<body>");
          out.println("<FORM name=statusmain>");
          out.println("<INPUT type=\"hidden\" name=\"id\">");
          out.println("<H2>Frames Status Reporting -- Task Listing</H2>");
          out.println("<P>Select one of the tasks below to view the weekly status reports:<BR>");
          out.println("(Blue = Changed this week / Black = unchanged from last week / Red = no status)</P>");
          out.println("<TABLE border=1>");
          out.println(" <TR>");
          out.println("   <TH>Select</TH>");
          out.println("   <TH>Charge Code</TH>");
          out.println("   <TH>Status ID</TH>");
          out.println("   <TH>Name</TH>");
          out.println("   <TH>Task Lead</TH>");
          out.println(" </TR>");
          // set up loop to print out the returned results
          while(rsTasks.next()) {
            // extract the fields from the database resultset
            String strUID = rsTasks.getString("Unique_ID");
            String strID = rsTasks.getString("ID");
            String strsID = rsTasks.getString("Status_Id");
            String strName = rsTasks.getString("Name");
            String strTaskLead = rsTasks.getString("Task_Lead");
            String strCode = rsTasks.getString("Charge_Code");
            java.sql.Date dtChange = rsTasks.getDate("Report_Date");
            String strColor = "Black";
            long diff = 0;
            if(dtChange != null) {
              diff = System.currentTimeMillis() - dtChange.getTime();
              if((System.currentTimeMillis() - dtChange.getTime()) <  604800000) {
                strColor = "Blue";
              }
            } else {
              strColor = "Red";
            }

            // print out the fields for the current row into the html table
            out.println(" <TR>");
            out.println("   <TD><INPUT id=button1 type=button value=Details name=button1 LANGUAGE=javascript onclick=\"return button_onclick(" + strUID + ")\"></TD>");
            out.println("   <TD><a href=http://bss-fms.pnl.gov/pmr/default.asp?Tab=3&Object=" + strCode + "&Brdn=TRUE target=\"Status\">" + strCode + "</a></TD>");
            out.println("   <TD><a href=http://mepas.pnl.gov/Wiki/page.jsp?website=FWikiWiki&path=Task%20"+strsID +" target=\"Status\">" + strsID + "</a></TD>");
//            out.println("   <TD>" + strsID + "</TD>");
            out.println("   <TD><FONT color=\"" + strColor + "\">" + strName + "</FONT></TD>");
            out.println("   <TD>" + strTaskLead + "</TD>");
            out.println(" </TR>");
          }
          // finish of the html for the page
          out.println("</TABLE>");
          out.println("</FORM>");
          out.println("</BODY>");
          out.println("</HTML>");

        } catch (Exception e) {
            e.printStackTrace();
        }

        out.close();
    }

    /** Handles the HTTP <code>GET</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, java.io.IOException {
        processRequest(request, response);
    }

    /** Handles the HTTP <code>POST</code> method.
     * @param request servlet request
     * @param response servlet response
     */
    protected void doPost(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, java.io.IOException {
        processRequest(request, response);
    }

    /** Returns a short description of the servlet.
     */
    public String getServletInfo() {
        return "Short description";
    }

}

