#ifndef FRAMES2XML_H
#define FRAMES2XML_H

#include <iostream>
#include <fstream>
#include <string>
#include <time.h>
#include "..\Common Files\SystemDevC.h"
#include "..\Common Files\DataSetC.h"
#include "XMLOutput.h"

using namespace std;

#ifdef _DEBUG
 #define DEBUG_FUNCTION(n) cout << "::::" << n << endl;
 #define DEBUG_FUNCTION0(n) cout << "::::" << n << "()" << endl;
 #define DEBUG_FUNCTION1(n, a) cout << "::::" << n << "(" << a << ")" << endl;
 #define DEBUG_FUNCTION2(n, a, b) cout << "::::" << n << "(" << a << ", " << b << ")" << endl;
 #define DEBUG_FUNCTION3(n, a, b, c) cout << "::::" << n << "(" << a << ", " << b << ", " << c << ")" << endl;
 #define DEBUG_FUNCTION4(n, a, b, c, d) cout << "::::" << n << "(" << a << ", " << b << ", " << c << ", " << d << ")" <<  endl;
 #define DEBUG_FUNCTION5(n, a, b, c, d, e) cout << "::::" << n << "(" << a << ", " << b << ", " << c << ", " << d << ", " << e << ")" << endl;
 #define DEBUG_RETURN(v) cout << "  return:\t\t\t" << v << endl;
 #define DEBUG_VARIABLE(v) cout << "  " ## #v ## ":\t\t" << v << endl ;

#else
 #define DEBUG_FUNCTION(n)
 #define DEBUG_FUNCTION0(n)
 #define DEBUG_FUNCTION1(n, a)
 #define DEBUG_FUNCTION2(n, a, b)
 #define DEBUG_FUNCTION3(n, a, b, c)
 #define DEBUG_FUNCTION4(n, a, b, c, d)
 #define DEBUG_FUNCTION5(n, a, b, c, d, e)
 #define DEBUG_RETURN(v)
 #define DEBUG_VARIABLE(v)
#endif

#ifdef _DEBUG
#define DEBUG_PAUSE int n = 0; cin >> n;
#else
#define DEBUG_PAUSE
#endif

const int MAX=4096;

/**
 * class providing convinient functions to output frames
 * to xml.
 * the three main functions of this class are the constructor
 * openSystem(), and outputSimulation().  the constructor
 * takes an OutputStream as an argument, where it will
 * output it's xml data to.  the openSystem() takes a
 * string array, for parsing command line arguments.  this
 * is important because frames passes arguments to a program
 * so that it can gain access to a simulation, therefore
 * the key to opening the simulation is making sure the
 * arguments get passed to openSystem().  the third function
 * , outputSimulation(), outputs all modules and all data
 * about that module to xml format.  this function calls
 * sub functions, such as outputModule, outputSchemes, and
 * so on.  these sub output functions can be called
 * individually if desired.
 * the output tag elements are hardcoded, so it will not
 * be convinient to modify these if needed.
 */
class Frames2XML
{
protected:
    char **_arguments;
    int _arguments_length;

    ostream *stream;

    ///this variable will be set to false whenever output
    ///to the stream passed in the constructor fails.
    ///therefore, majority of functions will check this
    ///value and return it's opposite for notification
    ///of success
    bool outputFailed;
private:
    ///these are declared with class scope so that each function which
    ///uses them does not have to instantiate them
    XMLOutput xml;
    XMLTagAttributes atts;

    int PID;

    ///frames variables
    char _simulationPath[MAX];
    char _simulationFile[MAX];
    char _simulationModule[MAX];
    char _simulationDataset[MAX];

    ///keeps track if the close tag (</frames>) has already been output
    bool outputClosing;

    ///helper variables for outputing values of variables in a dataset
    int dimensionSize;
    int* dimensionIndexArray;
    char _dictionary[MAX];
    char _indices[10][MAX];
    int _indexCount;
    char _dataset[MAX];
    char _variable[MAX];
    bool _vector;

public:
    /**
     * the constructor accepting a stream to write the xml
     * output to.  this parameter is required in order to
     * create an instance of this class.
     *
     * the constructor outputs an xml header declaring version
     * 1.0, and also instructs the invoker of this xml file
     * to use the xsl template, if the xsl parameter is not null.
     *
     * @param stream output stream in which this class
     * outputs frames objects to in xml format.
     */
    Frames2XML(ostream *stream, const char* xsl=0);
    
    /**
     * ensures that the close tag is output, if it has not been
     * already.
     */
    ~Frames2XML();

    /**
     * closes the xml document.  note that after calling this
     * function, any calls to functions which output more data
     * to the xml document will corrupt it's well-formedness.
     *
     * this function was provided as convinience in case the
     * user did not wish to wait for the destructor to write
     * out the close tag.
     */
    void closeTag();

    /**
     * returns the current PID used for accessing the FRAMES API
     */
    int getPID();

    /**
     * checks for invalid variables used to call the frames api,
     * such as PID and simulationDataset.  if anything is invalid
     * then return is true.  otherwise, return is false to
     * indicate no errors.
     */
    bool hasErrors();

    /**
     * output a header before output simulation.
     * currently only outputs date and time.
     */
    bool outputHeader();

    /**
     * convinience function for outputting the arguments passed
     * to the program by frames.  output will be to the stream
     * specified in the constructor of this class, in xml
     * format.
     *
     * @return true if success, false otherwise
     */
    bool outputArguments();

    /**
     * this is useful for outputting something to the xml.
     * it is mostly useful for outputting error tags with
     * the error message inside.
     *
     * @param tag the name of the tag which describes the message
     * @param message the message to output inbetween the xml tags
     *
     * @return false if output to stream failed
     */
    bool outputMessage(char *tag, char * message);

    /**
     * using the arguments frames passed to us, open up the system
     * and get our PID so that we can start using the API and
     * retrieving data.
     *
     * @param arguments arguments that were passed by frames 
     * @param count specifies how many arguments there are
     * @return true if open is successful, false otherwise
     */
    bool openSystem(char **arguments, int count);

    /**
     * output the entire simulation.  this outputs data about a simulation,
     * as well as every module and it's data in the simulation.
     *
     * @return returns success bool
     */
    bool outputSimulation();

    /**
     * helper function which holds the code to obtain the number of modules
     * in the open simulation.
     *
     * @return the count of modules in the simulation.  -1 if errors occured
     */
    int getNumberModules();

    void outputModuleHeader(int index);
    void outputModuleFooter();
    /**
     * each module in a simulation has an index which is the position of the
     * module in the module list.  using this index is the easiest way to
     * output a module, because with this index, other module data can be
     * easily obtained as well, such as the module label, name, class, group,
     * subgroup, and so on.  therefore, if you know the index of the module
     * this function will be more efficient than calling outputModule(String)
     *
     * outputs a module and it's schemes, UIIO, and metadata.
     *
     * @return returns success boolean
     */
    bool outputModule(int index);

    /**
     * if you know the name of a module, but not it's index, you can call
     * this function.  this function will loop through all the indexes until
     * it finds the module name with this name.  therefore this function
     * will be quite inefficient if used several times.  if you want to
     * output every module, then use getModuleCount() and outputModule(int).
     *
     * @param moduleId the simulation Id of the module to output.
     *
     * @return returns success boolean.  false if moduleName not found.
     */
    bool outputModule(char * moduleId);

    /**
      * using the module id, get the name/type of the module it represents
      * and store it in moduleName
      */
    void getNameFromId(char *moduleId, char *moduleName);

    /**
      * return the index of the moduleid in the simulation module list.
      */
    int getIndexFromId(char *moduleId);

    /**
     * outputs the user input input/output dictionary and it's variables
     * and values of those variables.
     *
     * @param moduleID the string of the moduleID in the simulation, in
     * which to output it's UIIO.
     *
     * @return returns success boolean.
     */
    bool outputModuleUIIO(char * moduleId);

    /**
     * outputs the input/output scheme for the module identified
     * by moduleId.  each module may have several schemes.  only
     * the scheme being used in the simulation is output.
     *
     * @param moduleId is the simulation Id, which is an instance
     * of a module, which holds the scheme that is being used
     * in the simulation
     *
     * @return returns success boolean
     */
    bool outputModuleScheme(char * moduleId);

    /**
     * outputs the simulation input or output dictionaries and their
     * current values, depending on the inputs boolean parameter.
     *
     * @param moduleId the input or output scheme of this module instance
     * that will be output.
     * @param inputs if true outputs the modules inputs.  if false, outpus
     * the module's outputs.
     *
     * @return returns success boolean
     */
    bool outputModuleIO(char * moduleId, bool inputs);

    /**
     * outputs the metadata of a module.  the metadata contains such
     * data as reference info and computer requirements.
     *
     * @param moduleName the name, not id, of a module in which to
     * output it's metadata.
     *
     * @return returns success boolean
     */
    bool outputModuleMetadata(char * moduleName);

    /**
     * function to output a variable of a module.
     * this function does not have to be used with
     * outputModuleMetadata.
     * the variable output belongs to the module
     * metadata.  this function has nothing to do
     * with dictionary variables.
     *
     * @param moduleName the name of the module
     * which contains the variable to output.
     * @param variable the variable which belongs
     * to the module metadata
     * @param type the type of this variable.
     * choices are STRING, DOUBLE, INTEGER, and
     * BOOLEAN
     * @param count if variable is a vector and
     * has multiple data values, count specifies
     * how many of these values should be output.
     *
     * @return returns success boolean
     */
    bool outputModuleMetadataHelper(char * moduleName, char * variableName, int type, int count);

    /**
     * outputs a dictionary and all it's variables.  values of
     * those variables are also output if dataset is not null.
     *
     * @param dictionary outputs this dictionary and it's variables.
     * @param dataset contains the values of the variables defined by dictionary
     * variables specified in dictionary are output.
     *
     * @return returns success boolean
     */
    bool outputDictionary(char * dictionary, char * dataset);

    /**
     * outputs variable belonging to a dictionary and it's values
     * if a dataset is specified.
     *
     * @param dictionary outputs the variable of this dictionary
     * @param dataset if this param is not null, values of the
     * variable specified in the variable parameter are output
     * @param variable the variable and it's values to output
     * 
     * @return returns success boolean
     */
    bool outputVariable(char * dictionary, char * dataset, char * variable);

    /**
     * outputs values of a variable belonging to a dataset.  note that
     * the output could be quite large with a possible 6 dimensional
     * variable.
     *
     * @param dictionary the dictionary which defines the dataset
     * @param dataset the dataset which holds the variable
     * @param variable ALL values of this variable will be output.
     *
     * @return returns success boolean
     */
    bool outputValues(char * dictionary, char * dataset, char * variable);

    /**
     * this function would is very unstable if called anywhere other than
     * outputValues().  it is used for recursing through multiple
     * dimensional variables, and outputting their values in a structured
     * xml format, so that the values of variables, and the dimensions
     * they were contained in can be easily reproduced.
     *
     * @param dimensionIndex holds the value of the current dimension
     * that should be processed, for example the third or the fourth.
     * even though frames is 1 based, dimensionIndex is 0 based.
     */
     void recurseDimension(int dimensionIndex);

     /**
      * outputs any references associated with the variable in dataset
      * at indices _1 through six.  For just the variable, it would be
      * smart to use indices 0,0,0,0,0,0.  you can actually use any
      * numbers you want, or negative numbers to associate with 
      * just the variable.
      */
     void outputReferencesForVariable(char * dataset, char* variable,
                                       int _1=0, int _2=0, int _3=0, int _4=0,
                                       int _5=0, int _6=0);
     /**
      * output the references dataset.  this function is not called
      * inside of outputSimulation, like most other functions.  this
      * is because references belong to all simulations.
      */
    void outputReferences();

    /**
      * return the module name passed to this program.
      */
    char* passedModuleName()
    {
      return _simulationModule;
    }
protected:

    /**
     * convinience func for turning a number of an index into a value
     */
    void getStringAtVariableIndex(int dimIndex, int index, char* value);

    /**
     * convinience func for retrieving value at the current dimension
     * index array in the recurseDimension
     */
    void getStringAtDimensionIndex(char* dic, char* dataset, char* var,
                                   int* indices, char* stringValue);


};

#endif