//
// Created by mhaidl on 29/05/16.
//

#include <fstream>
#include <iostream>
#include <cstdlib>


#include "Common.h"

namespace pacxx{
    namespace common
    {
        std::string GetEnv(const std::string &var) {
            const char *val = ::getenv(var.c_str());
            if (val == 0) {
                return "";
            } else {
                return val;
            }
        }


        std::string replace_substring(std::string subject,
                                             const std::string &search,
                                             const std::string &replace) {
            size_t pos = 0;
            while ((pos = subject.find(search, pos)) != std::string::npos) {
                subject.replace(pos, search.length(), replace);
                pos += replace.length();
            }
            return subject;
        }

      // reads the content of a file into a string
      std::string read_file(std::string filename) {
        std::ifstream in(filename, std::ios::in | std::ios::binary);
        if (in) {
          std::string content;
          in.seekg(0, std::ios::end);
          content.resize(in.tellg());
          in.seekg(0, std::ios::beg);
          in.read(&content[0], content.size());
          in.close();
          return content;
        }
        return "";
      }

      void write_string_to_file(const std::string& filename, const std::string& data) {

        std::fstream out(filename, std::ios::out);
        out << data;
        out.close();
      }

    }
}
