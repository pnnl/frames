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
public class WeeklyReport extends HttpServlet {

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

        // now create a statement to extract the data for the report
        Statement st = con.createStatement();
        String strSQL = "SELECT " +
            "t.Unique_ID, " +
            "t.ID, " +
            "t.Status_Id, " +
            "t.Name, " +
            "t.Task_Lead, " +
            "t.Notes, " +
            "t.Start_Date, " +
            "t.Finish_Date, " +
            "t.Predecessors, " +
            "t.Successors, " +
            "t.Authorized, " +
            "w.Report_Date, " +
            "w.ID as Status_ID, " +
            "ww.Work_Performed, " +
            "ww.Work_Due_But_Not_Completed, " +
            "ww.Problems, " +
            "ww.Suggested_Solutions, " +
            "ww.Dollars_Spent " +
          "FROM " +
            "(Tasks t LEFT OUTER JOIN " +
            "(" +
              "SELECT w.Task_Id, " +
                    "max(w.Report_Date) as Report_Date, " +
                    "max(w.ID) as ID " +
              "FROM   WeeklyStatus w " +
              "WHERE  w.Report_Date = " +
              "(" +
                "SELECT max(w2.Report_Date) " +
                "FROM   WeeklyStatus  w2 " +
                "WHERE w2.Task_Id = w.Task_ID " +
              ") " +
              "GROUP by w.Task_Id " +
            ") w on t.unique_id = w.task_id" +
            ") INNER Join WeeklyStatus ww ON w.ID= ww.ID " +
          "ORDER BY " +
            "t.Status_Id";
        // execute the SQL statemnent and return the results
        ResultSet rsTasks = st.executeQuery(strSQL);

        // now print out the html page
        out.println("<html>");
        out.println("<head>");
        out.println("<title>Frames Status Reporting--Weekly Task Report</title>");
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
        out.println("<H2>Frames Status Reporting -- Weekly Status Report</H2>");
        SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
        while(rsTasks.next()) {
          // here we can pull out the fields from the result set
          String strTID = rsTasks.getString("ID");
          String strSID = rsTasks.getString("Status_Id");
          String strTName = rsTasks.getString("Name");
          String strTLead = rsTasks.getString("Task_Lead");
          String strTDesc = rsTasks.getString("Notes");
          String strTStart = sdFormat.format(rsTasks.getDate("Start_Date"));
          String strTFinish = sdFormat.format(rsTasks.getDate("Finish_Date"));
          String strTPred = rsTasks.getString("Predecessors");
          String strTSucc = rsTasks.getString("Successors");
          double dblAuth = rsTasks.getDouble("Authorized");
          String strReport = sdFormat.format(rsTasks.getDate("Report_Date"));
          String strTWPerformed = rsTasks.getString("Work_Performed");
          String strTWDueBNC = rsTasks.getString("Work_Due_But_Not_Completed");
          String strTProblems = rsTasks.getString("Problems");
          String strTSolutions = rsTasks.getString("Suggested_Solutions");
          double dblSpent = rsTasks.getDouble("Dollars_Spent");
          // now output the html for the entry
          out.println("<TABLE border = 1 width=100%>");
          out.println(" <TR>");
          out.println("   <TH width=\"10%\">Report Date:</TH>");
          out.println("   <TD>" + strReport + "</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Task Lead:</TH>");
          out.println("   <TD>" + strTLead + "</TD>");
          out.println(" </TR>");
/*          out.println(" <TR>");
          out.println("   <TH>ID Number:</TH>");
          out.println("   <TD>" + strTID + "</TD>");
          out.println(" </TR>");
*/          out.println(" <TR>");
          out.println("   <TH>Task Number:</TH>");
          out.println("   <TD>" + strSID + "</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Task Title:</TH>");
          out.println("   <TD>" + strTName + "</TD>");
          out.println(" </TR>");
/*          out.println(" <TR>");
          out.println("   <TH>Task Description:</TH>");
          out.println("   <TD>" + strTDesc + "</TD>");
          out.println(" </TR>");
*/          out.println(" <TR>");
          out.println("   <TH>Scheduled Start:</TH>");
          out.println("   <TD>" + strTStart + "</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Scheduled Finish:</TH>");
          out.println("   <TD>" + strTFinish + "</TD>");
          out.println(" </TR>");
/*          out.println(" <TR>");
          out.println("   <TH>Predecessors:</TH>");
          out.println("   <TD>" + strTPred + "</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Successors:</TH>");
          out.println("   <TD>" + strTSucc + "</TD>");
          out.println(" </TR>");
*/          out.println(" <TR>");
          out.println("   <TH>Work Performed:</TH>");
          out.println("   <TD>" + strTWPerformed + "&nbsp;</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Work Due but not completed:</TH>");
          out.println("   <TD>" + strTWDueBNC + "&nbsp;<TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Problems:</TH>");
          out.println("   <TD>" + strTProblems + "&nbsp;</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Suggested Solutions to Problems:</TH>");
          out.println("   <TD>" + strTSolutions + "&nbsp;</TD>");
          out.println(" </TR>");
          out.println(" <TR>");
          out.println("   <TH>Percent Spent:</TH>");
          String strPSpent = "0";
          if(dblAuth != 0) {
            double dblPSpent = (dblSpent / dblAuth) * 100;
            strPSpent = Double.toString(dblPSpent);
          }
          out.println("   <TD>" + strPSpent + "%</TD>");
          out.println(" </TR>");
          out.println("</TABLE>");
          out.println("<BR><BR>");
        }
        // finish of the html for the page
        out.println("</TABLE>");
        out.println("</FORM>");
        out.println("</BODY>");
        out.println("</HTML>");
      }
      catch (Exception e) {
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
