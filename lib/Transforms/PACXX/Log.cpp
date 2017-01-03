//
// Created by mhaidl on 03/06/16.
//
#include "Log.h"
namespace pacxx
{
namespace common
{
//  static Log* the_log = nullptr;
//
//  __attribute__((constructor)) static void intializeLogging() {
//    the_log = new Log();
//  }
//
//  __attribute__((destructor)) static void shutdownLogging() {
//    delete the_log;
//  }

  Log& Log::get() {
    static Log the_log;
    return the_log;
  }

  Log::Log() : _silent(false), _no_warnings(false), output(std::cout) {
    _old_buffer = output.rdbuf();
    auto str = GetEnv("PACXX_LOG_LEVEL");
    log_level = 0;
    if (str.length() > 0) {
      log_level = std::stoi(str);
    }
  }

  Log::~Log() { resetStream(); }
}
}
