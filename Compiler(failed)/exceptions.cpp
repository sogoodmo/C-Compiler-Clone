#include "exceptions.hpp"

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
	int line;
	int col;

public:
	SemanticException(std::string err, int line, int col) : Err(err), line(line), col(col) {}

	virtual const char *what() const throw()
	{
		static std::string errMessage("\033[0;31mSemantic Error:\033[0m " + Err + " Line: " + std::to_string(line) + " Col: " + std::to_string(col));

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

public:
	ParseException(std::string err) : Err(err) {}

	virtual const char *what() const throw()
	{
		return Err.c_str();
	}
};
