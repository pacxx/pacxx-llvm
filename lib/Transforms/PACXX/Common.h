//
// Created by mhaidl on 29/05/16.
//

#ifndef PACXX_V2_COMMON_H
#define PACXX_V2_COMMON_H

#include <sstream>
#include <tuple>
#include <type_traits>

namespace pacxx {
namespace meta {

// meta programming to extract all values of an
// std::tuple and forward them to a lambad expression

template <size_t N> struct Apply {
  template <typename F, typename T, typename... A>
  static auto apply(F &&f, T &&t, A &&... a) {
    return Apply<N - 1>::apply(std::forward<F>(f), std::forward<T>(t),
                               std::get<N - 1>(std::forward<T>(t)),
                               std::forward<A>(a)...);
  }
};

template <> struct Apply<0> {
  template <typename F, typename T, typename... A>
  static auto apply(F &&f, T &&, A &&... a) {
    return std::forward<F>(f)(std::forward<A>(a)...);
  }
};

template <typename F, typename T> auto apply(F &&f, T &&t) {
  return Apply<std::tuple_size<std::decay_t<T>>::value>::apply(
      std::forward<F>(f), std::forward<T>(t));
}
/////////////////////////////////////////////////////////////////////////////

// applys a unary lambda function to each element in a tuple
template <typename F, size_t index> struct ForEach {
  template <typename... Ts>
  void operator()(F func, const std::tuple<Ts...> &t) {
    ForEach<F, index - 1>()(func, t);
    func(std::get<index - 1>(t));
  }
};

template <typename F> struct ForEach<F, 0> {
  template <typename... Components>
  void operator()(F func, const std::tuple<Components...> &t) {}
};

template <class F, class... Ts>
void for_each_in_tuple(F func, const std::tuple<Ts...> &t) {
  ForEach<F, sizeof...(Ts)>()(func, t);
}
/////////////////////////////////////////////////////////////////////////////
} /*namespace meta*/

namespace common {
template <typename F, typename... Ts> auto apply(F &&f, std::tuple<Ts...> &t) {
  return meta::apply(f, t);
}

template <typename F, typename... Ts>
void for_each_in_arg_pack(F &&f, Ts &&... t) {
  meta::for_each_in_tuple(std::forward<F>(f), std::forward_as_tuple(t...));
};

  template<typename F, typename... Ts>
  void for_first_in_arg_pack(F&& f, Ts&& ... t) {
    auto tpl = std::tie(t...);
    f(std::get<0>(tpl));
  };

// reads a environment variable
std::string GetEnv(const std::string &var);

// extracts the filename from a filepath

inline std::string get_file_from_filepath(std::string path) {

#ifndef __WIN32__
    std::string delim("/");
#else
    std::string delim("\\");
#endif
    std::string filename;

    size_t pos = path.find_last_of(delim);
    if (pos != std::string::npos)
      filename.assign(path.begin() + pos + 1, path.end());
    else
      filename = path;

    return filename;
  }

// replaces as substring in the subject string
std::string replace_substring(std::string subject, const std::string &search,
                              const std::string &replace);

// reads the content of a file into a string
std::string read_file(std::string filename);

  // write a string to a file
void write_string_to_file(const std::string& filename, const std::string& data);

  // pushes every input to a string stream and returns the string
template <typename... T> std::string to_string(T &&... args) {
  std::stringstream ss;

  for_each_in_arg_pack([&](auto &&arg) { ss << arg; },
                       std::forward<T>(args)...);

  return ss.str();
}
}
}

#endif // PACXX_V2_COMMON_H
