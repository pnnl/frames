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
public class WeeklyEntry extends HttpServlet {

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
        String strID = request.getParameter("id");
        String strTaskID = request.getParameter("taskid");
        String strAction = request.getParameter("action");

        // next we can look to see if we need to do a save or not (based on the action field)
        if(strAction != null) {
          // here we need to insert a record into the weekly status table
          String strReportDate = request.getParameter("reportdate");
          String strWorkPerformed = request.getParameter("workperformed");
          String strWorkDueButNotComplete = request.getParameter("workduebutnotcomplete");
          String strProblems = request.getParameter("problems");
          String strSuggested = request.getParameter("suggested");
          String strDollars = request.getParameter("dollars_spent");
          // check to see if we are doing an insert or an update
          if(strID == null || strID.equalsIgnoreCase("")) {
            // here we are doing an insert
            String strSQL = "INSERT INTO " +
                      "WeeklyStatus (" +
                        "Task_ID, " +
                        "Report_Date, " +
                        "Work_Performed, " +
                        "Work_Due_But_Not_Completed, " +
                        "Problems, " +
                        "Suggested_Solutions, " +
                        "Dollars_Spent) " +
                    "VALUES (?,?,?,?,?,?,?)";
            // create a prepared statement to execute the insert
            PreparedStatement insertWeekly = con.prepareStatement(strSQL);
            // set the parameters for the prepared statement
            insertWeekly.setInt(1,Integer.parseInt(strTaskID));
            SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
            java.util.Date dt = sdFormat.parse(strReportDate);
            insertWeekly.setDate(2,new java.sql.Date(dt.getTime()));
            insertWeekly.setString(3,strWorkPerformed);
            insertWeekly.setString(4,strWorkDueButNotComplete);
            insertWeekly.setString(5,strProblems);
            insertWeekly.setString(6,strSuggested);
            insertWeekly.setFloat(7,Float.parseFloat(strDollars));
            // execute the insert
            insertWeekly.executeUpdate();
          }
          else {
            // here we need to do an update
            String strSQL = "UPDATE " +
                      "WeeklyStatus " +
                    "SET " +
                      "Task_ID = ?, " +
                      "Report_Date = ?, " +
                      "Work_Performed = ?, " +
                      "Work_Due_But_Not_Completed = ?, " +
                      "Problems = ?, " +
                      "Suggested_Solutions = ?, " +
                      "Dollars_Spent = ? " +
                    "WHERE " +
                      "ID = ?";
            // create a prepared statement to execute the update
            PreparedStatement updateWeekly = con.prepareStatement(strSQL);
            // set the parameters for the prepared statement
            updateWeekly.setInt(1,Integer.parseInt(strTaskID));
            SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
            java.util.Date dt = sdFormat.parse(strReportDate);
            updateWeekly.setDate(2,new java.sql.Date(dt.getTime()));
            updateWeekly.setString(3,strWorkPerformed);
            updateWeekly.setString(4,strWorkDueButNotComplete);
            updateWeekly.setString(5,strProblems);
            updateWeekly.setString(6,strSuggested);
            updateWeekly.setFloat(7,Float.parseFloat(strDollars));
            updateWeekly.setInt(8,Integer.parseInt(strID));
            // execute the update
            updateWeekly.executeUpdate();
          }
          // now display some html
          out.println("<html>");
          out.println("<head>");
          out.println("<title>Frames Status Reporting--Weekly Status</title>");
          out.println("<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>");
          out.println("<!--");
          out.println("");
          out.println("function add_onclick() {");
          out.println("   window.document.taskdetail.method = \"POST\";");
          out.println("   window.document.taskdetail.action = \"TaskDetail\";");
          out.println("   window.document.taskdetail.submit();");
          out.println("}");
          out.println("");
          out.println("//-->");
          out.println("</SCRIPT>");
          out.println("</head>");
          out.println("<body>");
          out.println("<FORM name=taskdetail>");
          out.println("<INPUT type=\"hidden\" name=\"id\" value=\"" + strTaskID + "\">");
          out.println("<H2>Frames Status Reporting -- Weekly Status Entry</H2>");
          out.println("<P>Save was successful</P>");
          out.println("<P><INPUT id=add type=button value=Continue name=add LANGUAGE=javascript onclick=\"return add_onclick()\"></P>");
          out.println("</FORM>");
          out.println("</BODY>");
          out.println("</HTML>");
        }
        else {
        // here we need to print out the edit form
          if(strID != null && !(strID.equals(""))) {
            String strSQL = "SELECT " +
                      "w.Report_Date, " +
                      "t.Task_Lead, " +
                      "t.ID, " +
                      "t.Status_Id, " +
                      "t.Name, " +
                      "t.Authorized, " +
                      "t.Charge_Code, " +
                      "t.Notes, " +
                      "t.Start_Date, " +
                      "t.Finish_Date, " +
                      "t.Predecessors, " +
                      "t.Successors, " +
                      "w.Work_Performed, " +
                      "w.Work_Due_But_Not_Completed, " +
                      "w.Problems, " +
                      "w.Suggested_Solutions, " +
                      "w.Dollars_Spent " +
                "FROM " +
                  "Tasks t, WeeklyStatus w " +
                "WHERE " +
                  "t.Unique_ID = w.Task_ID " +
                "AND " +
                  "w.Task_ID = ? " +
                "AND " +
                  "w.ID = ?";
            PreparedStatement getWeekly = con.prepareStatement(strSQL);
            // set the parameters for the prepared statement
            getWeekly.setInt(1,Integer.parseInt(strTaskID));
            getWeekly.setInt(2,Integer.parseInt(strID));
            ResultSet rsWeekly = getWeekly.executeQuery();
            rsWeekly.next();

            out.println("<html>");
            out.println("<head>");
            out.println("<title>Frames Status Reporting--Weekly Status</title>");
            out.println("</head>");
            out.println("<body>");
            out.println("<FORM name=weeklystatus method=POST action=WeeklyEntry>");
            out.println("<INPUT type=\"hidden\" name=\"id\" value=\"" + strID + "\">");
            out.println("<INPUT type=\"hidden\" name=\"taskid\" value =\"" + strTaskID + "\">");
            out.println("<INPUT type=\"hidden\" name=\"action\" value=\"save\">");
            out.println("<H2>Frames Status Reporting -- Weekly Status Listing</H2>");
            out.println("<TABLE border = 1>");
            out.println(" <TR>");
            out.println("   <TH>Report Date (MM/DD/YYYY):</TH>");
            SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
            out.println("   <TD><INPUT NAME=\"reportdate\" VALUE=\"" + sdFormat.format(rsWeekly.getDate("Report_Date")) + "\" SIZE=30></TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Task Lead:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Task_Lead") + "</TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Id Number:</TH>");
            out.println("   <TD>" + rsWeekly.getString("ID") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Task Number:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Status_Id") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Task Title:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Name") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Authorized:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Authorized") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Charge Code:</TH>");
            String strCode = rsWeekly.getString("Charge_Code");
            out.println("   <TD><a href=http://bss-fms.pnl.gov/pmr/default.asp?Tab=3&Object=" + strCode + "&Brdn=TRUE target=\"Status\">" + strCode + "</a></TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Task Description:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Notes") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Scheduled Start:</TH>");
            out.println("   <TD>" + sdFormat.format(rsWeekly.getDate("Start_Date")) + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Scheduled Finish:</TH>");
            out.println("   <TD>" + sdFormat.format(rsWeekly.getDate("Finish_Date")) + "</TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Predecessors:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Predecessors") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Successors:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Successors") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Work Performed:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"workperformed\" ROWS=10 COLS=100>" + rsWeekly.getString("Work_Performed") + "</TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Work Due but not completed:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"workduebutnotcomplete\" ROWS=10 COLS=100>" + rsWeekly.getString("Work_Due_But_Not_Completed") + "</TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Problems:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"problems\" ROWS=5 COLS=100>" + rsWeekly.getString("Problems") + "</TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Suggested Solutions to Problems:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"suggested\" ROWS=5 COLS=100>" + rsWeekly.getString("Suggested_Solutions") + "</TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Dollars Spent (Number--Required):</TH>");
            out.println("   <TD><INPUT NAME=\"dollars_spent\" SIZE=30 VALUE=\"" + rsWeekly.getString("Dollars_Spent") + "\"></TD>");
            out.println(" </TR>");
            out.println("</TABLE>");
            out.println("<P><INPUT id=submit1 type=submit value=Submit name=submit1></P>");
            out.println("</FORM>");
            out.println("</BODY>");
            out.println("</HTML>");
          }
          else {
            String strSQL = "SELECT " +
                      "Task_Lead, " +
                      "ID, " +
                      "Status_Id, " +
                      "Name, " +
                      "Authorized, " +
                      "Charge_Code, " +
                      "Notes, " +
                      "Start_Date, " +
                      "Finish_Date, " +
                      "Predecessors, " +
                      "Successors " +
                "FROM " +
                  "Tasks " +
                "WHERE " +
                  "Unique_ID = ?";
            PreparedStatement getWeekly = con.prepareStatement(strSQL);
            // set the parameters for the prepared statement
            getWeekly.setInt(1,Integer.parseInt(strTaskID));
            ResultSet rsWeekly = getWeekly.executeQuery();
            rsWeekly.next();

            out.println("<html>");
            out.println("<head>");
            out.println("<title>Frames Status Reporting--Weekly Status</title>");
            out.println("</head>");
            out.println("<body>");
            out.println("<FORM name=weeklystatus method=POST action=WeeklyEntry>");
            // out.println("<INPUT type=\"hidden\" name=\"id\" value=\"" + strID + "\">");
            out.println("<INPUT type=\"hidden\" name=\"action\" value=\"save\">");
            out.println("<INPUT type=\"hidden\" name=\"taskid\" value =\"" + strTaskID + "\">");
            out.println("<H2>Frames Status Reporting -- Weekly Status Listing</H2>");
            out.println("<TABLE border = 1>");
            out.println(" <TR>");
            out.println("   <TH>Report Date (MM/DD/YYYY):</TH>");
            SimpleDateFormat sdFormat = new SimpleDateFormat("MM/dd/yyyy");
            out.println("   <TD><INPUT NAME=\"reportdate\" SIZE=30></TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Task Lead:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Task_Lead") + "</TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Id Number:</TH>");
            out.println("   <TD>" + rsWeekly.getString("ID") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Task Number:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Status_Id") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Task Title:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Name") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Authorized:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Authorized") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Charge Code:</TH>");
            String strCode = rsWeekly.getString("Charge_Code");
            out.println("   <TD><a href=http://bss-fms.pnl.gov/pmr/default.asp?Tab=3&Object=" + strCode + "&Brdn=TRUE target=\"Status\">" + strCode + "</a></TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Task Description:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Notes") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Scheduled Start:</TH>");
            out.println("   <TD>" + sdFormat.format(rsWeekly.getDate("Start_Date")) + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Scheduled Finish:</TH>");
            out.println("   <TD>" + sdFormat.format(rsWeekly.getDate("Finish_Date")) + "</TD>");
            out.println(" </TR>");
/*            out.println(" <TR>");
            out.println("   <TH>Predecessors:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Predecessors") + "</TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Successors:</TH>");
            out.println("   <TD>" + rsWeekly.getString("Successors") + "</TD>");
            out.println(" </TR>");
*/            out.println(" <TR>");
            out.println("   <TH>Work Performed:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"workperformed\" ROWS=10 COLS=100></TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Work Due but not completed:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"workduebutnotcomplete\" ROWS=10 COLS=100></TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Problems:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"problems\" ROWS=5 COLS=100></TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Suggested Solutions to Problems:</TH>");
            out.println("   <TD>");
            out.println("     <TEXTAREA NAME=\"suggested\" ROWS=5 COLS=100></TEXTAREA>");
            out.println("   </TD>");
            out.println(" </TR>");
            out.println(" <TR>");
            out.println("   <TH>Dollars Spent (Number--Required):</TH>");
            out.println("   <TD><INPUT NAME=\"dollars_spent\" SIZE=30></TD>");
            out.println(" </TR>");
            out.println("</TABLE>");
            out.println("<P><INPUT id=submit1 type=submit value=Submit name=submit1></P>");
            out.println("</FORM>");
            out.println("</BODY>");
            out.println("</HTML>");
          }
          con.close();
        }
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
