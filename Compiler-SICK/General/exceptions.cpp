#include "exceptions.hpp"

std::string filename; 
std::ifstream filereader; 


std::string Warning::to_string()
{
    return Err + ". Line: " + std::to_string(lineno) + " Col: " + std::to_string(colno);
};

const char *SemanticException::what() const throw()
{   
    //Colouring the error 
    std::string errCode = "\033[0;31mSemantic Error: \033[0m";

    std::string line;
    filereader.open(filename);
    int curLine = 0;

    while (curLine++ != lineNo)
    {
        getline(filereader, line);
    } 

    
    //Generating message to look like code snipped 
    static std::string errMessage(errCode + " " + Err + "\n\nGot the following: Line: " + std::to_string(lineNo) + "\n\n|\n" + std::to_string(lineNo) + "| " + "\033[0;31m" + line + "\033[0m" + "\n|\n");

    filereader.close();
    return errMessage.c_str();

};

const char *ParseException::what() const throw()
{
    std::string errCode = "\033[0;31m Parsing Error: \033[0m";

    std::string line;
    filereader.open(filename);
    int curLine = 0;

    while (curLine++ != lineNo)
    {
        getline(filereader, line);
    } 

    //Formatting string to look like error message 
    static std::string errMessage(errCode + " " + Err + "\n|\n" + std::to_string(lineNo) + "| " + "\033[0;31m\033[4m" + line + "\033[0m" + "\n|");
    
    filereader.close();
    return errMessage.c_str();
};