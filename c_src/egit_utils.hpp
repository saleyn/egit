#pragma once

#include <tuple>
#include <string>
#include <memory>
#include <type_traits>
#include <erl_nif.h>
#include "egit_atoms.hpp"

static ErlNifResourceType* GIT_REPO_RESOURCE;

struct GitRepoPtr {
  static GitRepoPtr* create(git_repository* p) {
    auto rp = static_cast<GitRepoPtr*>(enif_alloc_resource(GIT_REPO_RESOURCE, sizeof(GitRepoPtr)));

    #ifdef NIF_DEBUG
    fprintf(stderr, "=egit=> Allocated NIF resource %p (%p) [%d]\r\n", p, rp, __LINE__);
    #endif

    if (!rp) [[unlikely]]
      return nullptr;

    new (rp) GitRepoPtr(p);
    return rp;
  }

  ~GitRepoPtr() {
    if (m_ptr) {
      #ifdef NIF_DEBUG
      fprintf(stderr, "=egit=> Releasing repository pointer %p (%p) [%d]\r\n", m_ptr, this, __LINE__);
      #endif
      git_repository_free(m_ptr);
      m_ptr = nullptr;
    }
  }

  git_repository const* get() const { return m_ptr; }
  git_repository*       get()       { return m_ptr; }

  ERL_NIF_TERM to_enif_resource(ErlNifEnv* env) {
    ERL_NIF_TERM resource = enif_make_resource(env, (void*)this);

    #ifdef NIF_DEBUG
    fprintf(stderr, "=egit=> Moved repo ownership %p [%d]\r\n", this, __LINE__);
    #endif

    enif_release_resource((void*)this); // Grant ownership to the resource
    return resource;
  }

private:
  GitRepoPtr(git_repository* p) : m_ptr(p) {}

  git_repository* m_ptr;
};

/*
#define SMART_PTR(var, deleter) \
  std::unique_ptr<std::remove_pointer<decltype(var)>::type, void(*)(decltype(var))> p ## var(var, deleter)
*/

template <typename T>
struct SmartPtr {
  using type = T;
  typedef void(*Deleter)(T*);

  explicit SmartPtr(Deleter del, T* p = nullptr) : m_ptr(p), m_del(del) {
    assert(m_del);
  }

  ~SmartPtr() {
    if (m_ptr) { m_del(m_ptr); m_ptr = nullptr; }
  }

  T**      operator&()        { return &m_ptr; }
  T&       operator*()        { assert(m_ptr); return *m_ptr; }
  T const* operator->() const { return m_ptr;  }
  T*       operator->()       { return m_ptr;  }
  bool     operator!()  const { return m_ptr == nullptr; }

  // Cast operator
  operator const T*()   const { return (const T*)m_ptr;  }
  operator T*()         const { return m_ptr;            }
  operator bool()       const { return m_ptr != nullptr; }

  T const* get()        const { return m_ptr; }
  T*       get()              { return m_ptr; }

  // Release ownership
  T*       release()          { auto p = m_ptr; m_ptr = nullptr; return p; }

  void     swap(SmartPtr<T>& src) { std::swap(m_ptr, src.m_ptr); }

  template <typename U>
  U cast() { return reinterpret_cast<U>(m_ptr); }
private:
  T*      m_ptr;
  Deleter m_del;
};

template <typename T>
struct ScopeCleanup {
  ScopeCleanup(T fun) : m_fun(fun) {}
  ~ScopeCleanup() { m_fun(); }
private:
  T m_fun;
};

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

inline std::string bin_to_str(ErlNifBinary const& bin) {
  return std::string((const char*)bin.data, bin.size);
}

inline ERL_NIF_TERM oid_to_bin_term(ErlNifEnv* env, git_oid const* oid, int abbrev = GIT_OID_SHA1_HEXSIZE)
{
  assert(abbrev <= GIT_OID_SHA1_HEXSIZE);
  char buf[GIT_OID_SHA1_HEXSIZE+1];
  git_oid_tostr(buf, abbrev+1, oid);
  return make_binary(env, std::string_view(buf, abbrev));
}

inline std::string oid_to_str(git_oid const& oid, int abbrev = GIT_OID_SHA1_HEXSIZE)
{
  auto out = std::string(abbrev, '\0');
  git_oid_tostr(out.data(), abbrev+1, &oid);
  return out;
}

inline std::string oid_to_str(git_oid const* oid, int abbrev = GIT_OID_SHA1_HEXSIZE) {
  return oid_to_str(*oid, abbrev);
}

inline std::string oid_to_str(git_object const* oid, int abbrev = GIT_OID_SHA1_HEXSIZE) {
  return oid_to_str(git_object_id(oid), abbrev);
}

inline std::string atom_to_str(ErlNifEnv* env, ERL_NIF_TERM atom) {
  unsigned int len;
  if (!enif_get_atom_length(env, atom, &len, ERL_NIF_UTF8))
    return "<bad-atom-len>";
  std::string s(len, '\0');
  if (!enif_get_atom(env, atom, s.data(), len+1, ERL_NIF_UTF8)) [[unlikely]]
    return std::format("<bad-atom:{}>", atom);
  return s;
}

inline ERL_NIF_TERM fmt_git_error(ErlNifEnv* env, std::string const& pfx)
{
  char        buf[256];
  const char* delim = "";
  const char* err   = "";

  if (git_error_last())
    err = git_error_last()->message;

  if (git_error_last() && !pfx.empty())
    delim = ": ";

  snprintf(buf, sizeof(buf), "%s%s%s", pfx.c_str(), delim, err);
  return make_binary(env, buf);
}

inline ERL_NIF_TERM raise_git_exception(ErlNifEnv* env, std::string const& pfx)
{
  return enif_raise_exception(env, fmt_git_error(env, pfx));
}

inline ERL_NIF_TERM raise_badarg_exception(ErlNifEnv* env, ERL_NIF_TERM err)
{
  return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, err));
}

inline ERL_NIF_TERM make_git_error(ErlNifEnv* env, std::string const& pfx)
{
  return enif_make_tuple2(env, ATOM_ERROR, fmt_git_error(env, pfx));
}

inline ERL_NIF_TERM make_error(ErlNifEnv* env, std::string_view const& err)
{
  return enif_make_tuple2(env, ATOM_ERROR, make_binary(env, err));
}

inline ERL_NIF_TERM make_error(ErlNifEnv* env, ERL_NIF_TERM err)
{
  return enif_make_tuple2(env, ATOM_ERROR, err);
}
