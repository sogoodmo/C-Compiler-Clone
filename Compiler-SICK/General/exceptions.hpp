#ifndef EXCEPTION_H
#define EXCEPTION_H

#include "imports.hpp"
//exceptions.hpp Header file for all exception classes (And warnings)


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

	std::string to_string();
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

	virtual const char *what() const throw();
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

	virtual const char *what() const throw();
};


#endif