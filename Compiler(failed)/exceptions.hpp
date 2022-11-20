#ifndef EXCEPTION_H
#define EXCEPTION_H

#include "imports.hpp"
//exceptions.hpp Header file for all exception classes (And warnings)


std::string filename; 
std::ifstream filereader; 


/**
 * Class to store warnings, but not crash program
 */
class Warning
{
	std::string Err;
	int lineno;
	int colno;

public:
	Warning(std::string err, int line, int col) : Err(err), lineno(line), colno(col) {}

	std::string to_string()
	{
		return Err + ". Line: " + std::to_string(lineno) + " Col: " + std::to_string(colno);
	}
};


/**
 * @brief Class for handling semenatic errors
 *
 */
class SemanticException : public std::exception
{
	std::string Err;
	int lineNo;
	int col;

public:
	SemanticException(std::string err, int lineNo, int col) : Err(err), lineNo(lineNo), col(col) {}

	virtual const char *what() const throw()
	{   
        //Colouring the error 
        std::string errCode = "\033[0;31m Semantic Error: \033[0m";

		filereader.open(filename);
		int curLine = 0;
		std::string line;

		while (curLine++ != lineNo)
		{
			getline(filereader, line);
		} 

        
        //Generating message to look like code snipped 
		std::string errMessage(errCode + " " + Err + "\n\nGot the following: Line: " + std::to_string(lineNo) + "\n\n|\n" + std::to_string(lineNo) + "| " + "\033[0;31m\033[4m" + line + "\033[0m" + "\n|\n");

		filereader.close();

		return errMessage.c_str();

	}
};


/**
 * @brief Custom exception class for parse errors
 *
 */
class ParseException : public std::exception
{
	std::string Err;
	int lineNo;
	int col;

public:
	ParseException(std::string err, int lineNo, int col) : Err(err), lineNo(lineNo), col(col) {}

	virtual const char *what() const throw()
	{
		std::string errCode = "\033[0;31m Parsing Error: \033[0m";

		filereader.open(filename);
		int curLine = 0;
		std::string line;

		while (curLine++ != lineNo)
		{
			getline(filereader, line);
		} 

        //Formatting string to look like error message 
		std::string errMessage(errCode + " " + Err + "\n|\n" + std::to_string(lineNo) + "| " + "\033[0;31m\033[4m" + line + "\033[0m" + "\n|");

		return errMessage.c_str();
	}
};


#endif