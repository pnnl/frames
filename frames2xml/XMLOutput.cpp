#include "XMLOutput.h"

void replace(char *str, int what, int with)
{
    char *c = str;
    while(c)
    {
        c = strchr(str, what);
        if(c)
            c[0] = with;
    }
}

void replace(char* target, const char* what, const char* with)
{
 int sizeWith = strlen(with);
 int sizeWhat = strlen(what);

 string res = target;
 for (int loc = res.find(what); loc != string::npos; loc = res.find(what, loc+sizeWith ) )
  res.replace( loc, sizeWhat, with);

 strcpy(target, res.c_str());

}

XMLOutput::XMLOutput()
{
	tabWidth = 1;
	tabCount = 0;

	onNewLine = true;
}
/*
void XMLOutput::open(char * tag)
{
	open(tag, null, false);
}
*/
void XMLOutput::opennl(char * tag)
{
	open(tag, NULL, true);
}
/*
void XMLOutput::open(char * tag, XMLTagAttributes *atts)
{
	open(tag, atts, false);
}
*/
void XMLOutput::opennl(char * tag, XMLTagAttributes *atts)
{
	open(tag, atts, true);
}

void XMLOutput::open(char * tag, XMLTagAttributes *atts, bool newline)
{
	///tab() knows to tab on new lines, and not to otherwise
	tab();

	///start the tag
	_output.append("<");
	_output.append(tag);
	
	///output attributes for that tag, if any
	if(atts)
	    _output.append(atts->attributes());

	///close the tag
	_output.append(">");

	///for every open tag, increase the tab
	tabCount++;

	///call outputNewLine if necessary because it needs to set
	///a flag to let tab know if it's on a new line or not
	if(newline)
	    outputNewLine();
}

void XMLOutput::putValue(char * value)
{
	putValue(value, false);
}

void XMLOutput::putValuenl(char * value)
{
	putValue(value, true);
}

void XMLOutput::putValue(char * value, bool newline)
{
	///tab() knows to tab on new lines, and not to otherwise
	tab();

  replace(value, '<', '[');
  replace(value, '>', ']');
  replace(value, "&", "&amp;");

	_output.append(value);

	///call outputNewLine if necessary because it needs to set
	///a flag to let tab know if it's on a new line or not
	if(newline)
	    outputNewLine();
}

void XMLOutput::close(char * tag)
{
	close(tag, false);
}

void XMLOutput::closenl(char * tag)
{
	close(tag, true);
}

void XMLOutput::close(char * tag, bool newline)
{
	///for every close tag, decrease the tab
	if(tabCount > 0)
	    tabCount--;

	///tab() knows to tab on new lines, and not to otherwise
	tab();

	_output.append("</");
	_output.append(tag);
	_output.append(">");

	///call outputNewLine if necessary because it needs to set
	///a flag to let tab know if it's on a new line or not
	if(newline)
	    outputNewLine();
}

void XMLOutput::clear()
{
    _output = "";
}

const char * XMLOutput::output()
{

	return _output.c_str();
}

void XMLOutput::setTabWidth(int width)
{
	if(width > 0)
	    tabWidth = width;
}

void XMLOutput::tab()
{
	///only tab on new lines
	if(onNewLine)
	{
	    onNewLine = false;
	
	    int width = tabCount*tabWidth;

	    for(int i = 0; i < width; i++)
	    	_output.append(" ");
	}
}

void XMLOutput::outputNewLine()
{
	onNewLine = true;
	_output.append("\r\n");
}

XMLTagAttributes::XMLTagAttributes()
{
}

const char * XMLTagAttributes::attributes()
{
    return _attributes.c_str();
}

void XMLTagAttributes::add(char * name, char * value)
{
    replace(value, '<', '[');
    replace(value, '>', ']');
    replace(value, "&", "&amp;");

    _attributes.append(" ");
	_attributes.append(name);
	_attributes.append("=");
	_attributes.append("\"");
	_attributes.append(value);
	_attributes.append("\"");
}

void XMLTagAttributes::clear()
{
	_attributes = "";
}
