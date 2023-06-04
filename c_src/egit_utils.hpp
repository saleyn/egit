#pragma once

#include <tuple>
#include <string>
#include <memory>
#include <type_traits>
#include <erl_nif.h>
#include "egit_atoms.hpp"

#define SMART_PTR(var, deleter) \
  std::unique_ptr<std::remove_pointer<decltype(var)>::type, void(*)(decltype(var))> p ## var(var, deleter)

inline std::tuple<ERL_NIF_TERM, unsigned char*>
make_binary(ErlNifEnv* env, size_t size)
{
  ERL_NIF_TERM term;
  auto   p = enif_make_new_binary(env, size, &term);
  return std::make_tuple(term, p);
}

inline ERL_NIF_TERM make_binary(ErlNifEnv* env, std::string_view const& str)
{
  auto [term, p] = make_binary(env, str.length());
  memcpy(p, str.data(), str.length());
  return term;
}

inline ERL_NIF_TERM fmt_git_error(ErlNifEnv* env, std::string const& pfx)
{
  char        buf[256];
  const char* delim = "";
  const char* err   = "";

  if (git_error_last()) {
    err   = git_error_last()->message;
    delim = ": ";
  }
  snprintf(buf, sizeof(buf), "%s%s%s", pfx.c_str(), delim, err);
  return make_binary(env, buf);
}

inline ERL_NIF_TERM raise_error(ErlNifEnv* env, std::string const& pfx)
{
  return enif_raise_exception(env, fmt_git_error(env, pfx));
}

inline ERL_NIF_TERM make_error(ErlNifEnv* env, std::string const& pfx)
{
  return enif_make_tuple2(env, ATOM_ERROR, fmt_git_error(env, pfx));
}

