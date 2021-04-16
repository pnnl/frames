#include <string>
#include <iostream>

using namespace std;
class XMLTagAttributes;

/**
 * this class does not output xml versions or output in certain
 * character sets.  it only outputs tags, attributes, and the
 * data in between tags.
 * this class makes it convinient to output xml.  it makes your
 * code easier to read, and automatically takes care of tabbing
 * and syntax.  at most, you only have tag names, attributes,
 * and newlines to worry about.
 * call the open functions to open a tag, and the putValue
 * functions to put data in between tags, and then close the
 * tag with one of the close functions.  call opennl or closenl
 * putValuenl to add a newline after the data is output.
 * after creating all your tags and data, call output() to grab
 * the xml output.  to clear the output, just call clear().
 * if you want to change the tab widthw call setTabWidth(int).
 * the default tab width is 1 space.
 * this class uses StringBuffer to minimize on the creating
 * of Strings, thus saving memory and optimizing speed.
 *
 * here is a simple example.
 *
 * XMLOutput out = new XMLOutput;
 * XMLTagAttributes atts = new XMLTagAttributes();
 *
 * atts.add("name", "berry");
 * out.opennl("student", atts);
 * out.open("grade");
 * out.putValue("98");
 * out.closenl("grade");
 * out.closenl("student");
 *
 * System.out.print(out.output());
 *
 * which outputs:
 *
 * <student name="berry">
 *  <grade>98</grade>
 * </student>
 */
class XMLOutput
{
protected:
    string _output;

    ///tab information
    int tabWidth;
    int tabCount;

    bool onNewLine;

public:
    XMLOutput();

    /**
     * @see open(char *, XMLTagAttributes, boolean)
     */
    //void open(char * tag);

    /**
     * @see open(char *, XMLTagAttributes, boolean)
     */
    void opennl(char * tag);

    /**
     * @see open(char *, XMLTagAttributes, boolean)
     */
    //void open(char * tag, XMLTagAttributes *atts);

    /**
     * @see open(char *, XMLTagAttributes, boolean)
     */
    void opennl(char * tag, XMLTagAttributes *atts);

    /**
     * this function opens a tag, with specified attributes atts, and
     * ends the tag with a newline if newline is true.
     *
     * @param tag don't include <'s or >'s, just the name of the tag
     *
     * @param atts can be null if there are none.  use the 
     * XMLTagAttributes class to build attributes and their values.
     * the attribute part of a tag, for example, is: name="berry"
     *
     * @param newline end tag in newline if true
     */
    void open(char * tag, XMLTagAttributes *atts = NULL, bool newline = false);

    /**
     * @see putValue(char *, boolean);
     */
    void putValue(char * value);

    /**
     * @see putValue(char *, boolean);
     */
    void putValuenl(char * value);

    /**
     * this function puts value in between tags.  set newline to
     * true if you want the next data or tags to appear on a
     * new line.  putValue does not increment the tab like open()
     * does.
     *
     * @param value the value that belongs in the middle of tags.
     *
     * @param newline end value in newline if true
     */
    void putValue(char * value, bool newline);

    /**
     * @see close(char *, boolean)
     */
    void close(char * tag);

    /**
     * @see close(char *, boolean)
     */
    void closenl(char * tag);

    /**
     * this function closes a tag, and ends the tag with a newline
     * if newline is true.
     *
     * @param tag don't include <'s or >'s, just the name of the tag
     *
     * @param newline end tag in newline if true
     */
    void close(char * tag, bool newline);

    /**
     * clears the output and resets the tabs to zero
     */
    void clear();

    /**
     * return the output char *
     */
    const char *output();

    /**
     * changes the tab width to width spaces.  it is best to call this
     * function before outputting anything, otherwise you might
     * see unexpected results.
     *
     * @param width the width to set each tab to.  default is 1, and
     * the tab width will not be changed if width < 1.
     */
    void setTabWidth(int width);

    /**
     * adds tab string to output, which is tabCount*tabWidth long of
     * spaces.  tabCount and tabWidth are internal variables, only
     * of which tabWidth can be set using setTabWidth().
     */
    void tab();

    /**
     * output a new line and put out a flag so that tab knows when
     * to indent or not
     */
    void outputNewLine();
};

/**
 * this class is to be used with XMLOutput, and is provided
 * for convinience, much like XMLOutput, for attaching
 * attributes and their values to open tags.
 * this class uses StringBuffer to minimize on the creating
 * of Strings, thus saving memory and optimizing speed.
 */
class XMLTagAttributes
{
protected:
    string _attributes;

public:
    XMLTagAttributes();

    const char *attributes();

    /**
     * adds an attribue name and it's value.  for example, a
     * call of add("name", "berry") would append
     * ' name="berry"', to the list of attributes (without
     * the single quotes of course).
     * an attribute cannot contain "<" or ">".  therefore,
     * all occurances of these will be replaced with "[" and
     * "]" respectivly.
     *
     * @param name name of the xml attribute
     * @param value value that belongs to this attribute
     */
    void add(char * name, char * value);

    void clear();
};
