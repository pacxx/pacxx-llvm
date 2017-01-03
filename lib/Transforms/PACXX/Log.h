/* Copyright (C) University of Muenster - All Rights Reserved
* Unauthorized copying of this file, via any medium is strictly prohibited
* Proprietary and confidential
* Written by Michael Haidl <michael.haidl@uni-muenster.de>, 2010-2014
*/

#pragma once

namespace kronos
{
	class Log;
#if USE_STANDALONE
	static int _kronos_debug_level = 1;
#else
	extern int _kronos_debug_level;
#endif
	
	struct LOG_LEVEL
	{

		class Log;

		enum LEVEL
		{
			info = 0,
			debug = 1,
			verbose = 2,
			warning = -1,
			exception = -2,
			error = -3,
			fatal = -4,
			none = -999
		};
	};
}

template <kronos::LOG_LEVEL::LEVEL debug_level = kronos::LOG_LEVEL::info, typename... Params> static void __sprint(const char* file, int line, Params&&... args);

#define __message(...)       __sprint<>(__FILE__, __LINE__, __VA_ARGS__)
#define __warning(...)       __sprint<kronos::LOG_LEVEL::warning>(__FILE__, __LINE__, __VA_ARGS__)
#define __error(...)         __sprint<kronos::LOG_LEVEL::error>(__FILE__, __LINE__, __VA_ARGS__)
#define __debug(...)         __sprint<kronos::LOG_LEVEL::debug>(__FILE__, __LINE__, __VA_ARGS__)
#define __verbose(...)       __sprint<kronos::LOG_LEVEL::verbose>(__FILE__, __LINE__, __VA_ARGS__)
#define __fatal(...)         __sprint<kronos::LOG_LEVEL::fatal>(__FILE__, __LINE__, __VA_ARGS__)
#define __exception(...) 	 __sprint<kronos::LOG_LEVEL::exception>(__FILE__, __LINE__, __VA_ARGS__)

//#include "includes.h"
#include "Singleton.h"
#include "Common.h"
#include "timing.h"
#include <string>
#include <iostream>
#include <sstream>


using namespace kronos::common; 

namespace kronos
{

class Log : public Singleton<Log>
{
public:
	friend class Singleton<Log>;

	Log(): _no_newline(false), _c_return(false), _silent(false), _no_warnings(false), _disable_info_label(false), output(std::cout)
	{
		_old_buffer = output.rdbuf();
	}

	virtual ~Log()
	{
		resetStream();
	}

	void break_line()
	{
		if (_silent)
			return;
		output << "------------------------------------------------------------------------" << std::endl;
	}

	void setStream(std::ostream& stream)
	{
		output.rdbuf(stream.rdbuf());
	}

	void resetStream(){ output.rdbuf(_old_buffer); }

	void blank_line()
	{
		if (_silent)
			return;
		output << std::endl;
	}

	void no_new_line()
	{
		_no_newline = true;
	}

	void c_return()
	{
		_c_return = true;
	}
	
	void setSilent(bool value)
	{
		_silent = value;
	}

	void disableWarnings(bool value)
	{
		_no_warnings = value;
	}
	
	void disableInfoLabel(bool value = true)
	{
		_disable_info_label = value;
	}

	template <LOG_LEVEL::LEVEL debug_level = LOG_LEVEL::info, typename... Params> void print(const char* file, int line, Params&&... args)
	{


		if (_silent)
			return;
		if (_kronos_debug_level < debug_level)
			return;

        std::stringstream ss;

		auto time = timer();
		auto diff = time - last_time; 
		last_time = time; 

		output << "["<< diff <<"ms\t" << common::get_file_from_filepath(file) << ":" << line <<"]\t"; 
	

		switch (debug_level)
		{
		case LOG_LEVEL::fatal:
			ss << "\033[5m\033[31mFATAL ERROR:\033[37;1m";
			break;
		case LOG_LEVEL::error:
			ss << "\033[31;1mERROR:\033[37;1m\t";
			break;
		case LOG_LEVEL::warning:
			if (_no_warnings)
				return;
			ss << "\033[33;1mWARNING:\033[37;1m";
			break;
		case LOG_LEVEL::info:
			if (!_disable_info_label)
				output << "\033[32mINFO:\033[37;1m\t";
			break;
		case LOG_LEVEL::debug:
			ss << "\033[36mDEBUG:\033[37;1m\t";
			break;
		case LOG_LEVEL::verbose:
			ss << "\033[37mVERBOSE:\033[37;1m";
			break;
		case LOG_LEVEL::exception:
			ss << "\033[31mEXCEPTION:\033[37;1m";
			break;
		default:
			break;
		}

		printValue<Params...>(ss, std::forward<decltype(args)>(args)...);
	}

private:

	template <typename T, typename... Params> void printValue(std::stringstream& ss, T&& first, Params&&... other)
	{
		ss << first;

		if (sizeof...(other)) 
		{ 
			printValue(ss, std::forward<decltype(other)>(other)...);
		}
		else 
		{ 
			printOptions(ss);
		}

	}

    void printValue(std::stringstream& ss) { }

    void printOptions(std::stringstream& ss)
	{
	
		std::string str = ss.str();
		ss.str("");		

		std::cout << common::replace_substring(str, "\n", "\n\t\t\t\t\t") << "\033[0m";
		if (!_no_newline)
			output << std::endl; 
		else
		{
			_no_newline = false;
			if (_c_return)
			{
				output << '\r';
				fflush(stdout);
				_c_return = false;
			}
		}
	}

private:

	bool _no_newline;
	bool _c_return;
	bool _silent;
	bool _no_warnings;
	bool _disable_info_label;
	
	common::clock timer;
	double last_time;

	std::ostream& output;
	std::streambuf* _old_buffer;
};

}/*namespace kronos*/

template <kronos::LOG_LEVEL::LEVEL debug_level, typename... Params> static void __sprint(const char* file, int line, Params&&... args)
{
	kronos::Log::getRef()->print<debug_level, Params...>(file, line, std::forward<decltype(args)>(args)...);
}


