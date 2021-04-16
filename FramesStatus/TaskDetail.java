package FramesStatus;

import javax.servlet.*;
import javax.servlet.http.*;
import java.util.*;
import java.text.*;

import java.sql.*;

/**
 * @author d3k076
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class TaskDetail extends HttpServlet {

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

          // next extract the selected id
          int intID = Integer.parseInt(request.getParameter("id"));

          // now create a prepared statement to extract the data for the table
          String strSQL = "SELECT " +
                    "* " +
                  "FROM " +
                    "Tasks " +
                  "WHERE " +
                    "Unique_ID = ?";
          PreparedStatement getTaskInfo = con.prepareStatement(strSQL);
          // load the parameter for the prepared statement
          getTaskInfo.setInt(1,intID);
          // return the results
          ResultSet rsTask = getTaskInfo.executeQuery();

          // we also need to return the list of weekly status reports
          strSQL = "SELECT " +
                "* " +
               "FROM " +
                "WeeklyStatus " +
               "WHERE " +
                "Task_ID = ? " +
               "ORDER BY " +
                "Report_Date DESC";
          // create a prepared statement to execute the query
          PreparedStatement getWeeklyInfo = con.prepareStatement(strSQL);
          // load hte parameter for the prepared statement
          getWeeklyInfo.setInt(1,intID);
          // return the results
          ResultSet rsWeekly = getWeeklyInfo.executeQuery();
          rsTask.next();

          // now print out the html page
          out.println("<html>");
          out.println("<head>");
          out.println("<title>Frames Status Reporting--Task Details</title>");
          out.println("<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>");
          out.println("<!--");
          out.println("");
          out.println("function button_onclick(uid) {");
      //out.println(" window.alert(\"This is a test \" + uid);");
      out.println(" window.document.taskdetail.id.value = uid;");
      //out.println(" window.alert(\"Hidden value is: \" + window.document.statusmain.id.value);");
      out.println("   window.document.taskdetail.method = \"POST\";");
      out.println("   window.document.taskdetail.action = \"WeeklyEntry\";");
      out.println("   window.document.taskdetail.submit();");
          out.println("}");
          out.println("");
          out.println("function add_onclick() {");
      out.println("   window.document.taskdetail.method = \"POST\";");
      out.println("   window.document.taskdetail.action = \"WeeklyEntry\";");
      out.println("   window.document.taskdetail.submit();");
          out.println("}");
          out.println("");
          out.println("//-->");
          out.println("</SCRIPT>");
          out.println("</head>");
          out.println("<body>");
          out.println("<FORM name=taskdetail>");
          out.println("<INPUT type=\"hidden\" name=\"taskid\" value=\"" + intID + "\">");
          out.println("<INPUT type=\"hidden\" name=\"id\">");
          out.println("<H2>Frames Status Reporting -- Weekly Status Listing</H2>");

          String strsID = rsTask.getString("Status_Id");
          out.println("<H3>Task ID: <a href=http://mepas.pnl.gov/Wiki/page.jsp?website=FWikiWiki&path=Task%20"+strsID +" target=\"Status\">" + strsID + "</a></H3>");

          out.println("<H3>" + rsTask.getString("Name") + "</H3>");
          out.println("<H3>Task Lead: " + rsTask.getString("Task_Lead") + "</H3>");

          String strCode = rsTask.getString("Charge_Code");
          out.println("   <H3>Work Package: <a href=http://bss-fms.pnl.gov/pmr/default.asp?Tab=3&Object=" + strCode + "&Brdn=TRUE target=\"Status\">" + strCode + "</a></H3>");

          out.println("<P>Select the \"Add\" button to add a new weekly status report or select a status report to view:</P>");
          out.println("<P><INPUT id=add type=button value=Add name=add LANGUAGE=javascript onclick=\"return add_onclick()\"></P>");
          out.println("<TABLE border=1>");
          out.println(" <TR>");
          out.println("   <TH>Select</TH>");
          out.println("   <TH>Report Date</TH>");
          out.println("   <TH>Work Performed</TH>");
          out.println("   <TH>Dollars Spent</TH>");
          out.println(" </TR>");
          // set up loop to print out the returned results
          SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
          while(rsWeekly.next()) {
            // print out the fields for the current row into the html table
            out.println(" <TR>");
            out.println("   <TD><INPUT id=button1 type=button value=Edit name=button1 LANGUAGE=javascript onclick=\"return button_onclick(" + rsWeekly.getString("ID") + ")\"></TD>");
            out.println("   <TD>" + sdFormat.format(rsWeekly.getDate("Report_Date")) + "</TD>");
            out.println("   <TD>" + rsWeekly.getString("Work_Performed") + "</TD>");
            out.println("   <TD>" + rsWeekly.getString("Dollars_Spent") + "</TD>");
            out.println(" </TR>");
          }
          // finish of the html for the page
          out.println("</TABLE>");
          out.println("<P><INPUT id=back type=button value=Main name=back LANGUAGE=javascript onclick=\"window.location.href = 'StatusMain';\"></P>");
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

