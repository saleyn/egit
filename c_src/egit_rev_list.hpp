//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/rev-list.c
//-----------------------------------------------------------------------------
// libgit2 "rev-list" - transform a rev-spec into a list
//-----------------------------------------------------------------------------
// Written by the libgit2 contributors
//
// To the extent possible under law, the author(s) have dedicated all copyright
// and related and neighboring rights to this software to the public domain
// worldwide. This software is distributed without any warranty.
//
// You should have received a copy of the CC0 Public Domain Dedication along
// with this software. If not, see
// <http://creativecommons.org/publicdomain/zero/1.0/>.
//-----------------------------------------------------------------------------
#pragma once

#include <git2/common.h>
#include <mutex>

static std::mutex s_walk_mutex;

static int push_commit(git_revwalk* walk, const git_oid* oid, bool hide)
{
  return hide ? git_revwalk_hide(walk, oid) : git_revwalk_push(walk, oid);
}

static int push_spec(git_repository* repo, git_revwalk* walk, const std::string& spec, bool hide)
{
  SmartPtr<git_object> obj(git_object_free);

  if (git_revparse_single(&obj, repo, spec.c_str()) != GIT_OK)
    return -1;

  return push_commit(walk, git_object_id(obj), hide);
}

static int push_range(git_repository* repo, git_revwalk* walk, const std::string& range, bool hide)
{
  git_revspec revspec;

  if (git_revparse(&revspec, repo, range.c_str()) != GIT_OK) [[unlikely]]
    return -1;

  SmartPtr<git_object> from(git_object_free, revspec.from);
  SmartPtr<git_object> to  (git_object_free, revspec.to);

  if (revspec.flags & GIT_REVPARSE_MERGE_BASE) {
    /* TODO: support "<commit>...<commit>" */
    return GIT_EINVALIDSPEC;
  }

  if (push_commit(walk, git_object_id(revspec.from), !hide) != GIT_OK)
    return -1;

  return push_commit(walk, git_object_id(revspec.to), hide);
}

static ERL_NIF_TERM revwalk_parse_revs(
  ErlNifEnv* env, git_repository* repo, git_revwalk* walk, ERL_NIF_TERM rev_specs, int limit)
{
  ERL_NIF_TERM spec;
  bool hide = false;

  int i = 0;

  while (enif_get_list_cell(env, rev_specs, &spec, &rev_specs) && i++ < limit) {
    ErlNifBinary bin;

    if (enif_is_identical(spec, ATOM_NOT)) {
      hide = !hide;
      continue;
    }
    else if (!enif_inspect_binary(env, spec, &bin)) [[unlikely]]
      return raise_badarg_exception(env, spec);

    auto offset = bin.size > 1 && bin.data[0] == '^' ? 1 : 0;

    std::string s((const char*)bin.data+offset, bin.size - offset);

    if (offset) {
      if (push_spec(repo, walk, s, !hide) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Cannot walk the spec " + s);
    } else if (s.find("..") != std::string::npos) {
      if (push_range(repo, walk, s, hide) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Cannot walk the range " + s);;
    } else {
      if (push_spec(repo, walk, s, hide) == GIT_OK) [[likely]]
        continue;

      git_oid oid;

    #ifdef GIT_EXPERIMENTAL_SHA256
      if (git_oid_fromstr(&oid, s.c_str(), GIT_OID_SHA1) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Cannot get oid from " + s);
    #else
      if (git_oid_fromstr(&oid, s.c_str()) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Cannot get oid from " + s);
    #endif

      if (push_commit(walk, &oid, hide) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Cannot walk commit " + s);
    }
  }

  return 0;
}

ERL_NIF_TERM lg2_rev_list(ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM revspecs, ERL_NIF_TERM opts)
{
  git_sort_t sort = GIT_SORT_NONE;
  int limit = INT32_MAX, abbrev = GIT_OID_SHA1_HEXSIZE;

  // Parse options
  {
    int arity, n;
    ERL_NIF_TERM opt;
    const ERL_NIF_TERM* tagvals;

    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if      (enif_is_identical(opt, ATOM_TOPO_ORDER)) sort = git_sort_t((int)sort | GIT_SORT_TOPOLOGICAL);
      else if (enif_is_identical(opt, ATOM_DATE_ORDER)) sort = git_sort_t((int)sort | GIT_SORT_TIME);
      else if (enif_is_identical(opt, ATOM_REVERSE))    sort = git_sort_t((int)sort | ((sort & ~GIT_SORT_REVERSE) ^ GIT_SORT_REVERSE));
      else if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
        if      (enif_is_identical(tagvals[0], ATOM_LIMIT)  && enif_get_int(env, tagvals[1], &n) && n > 0) limit = n;
        else if (enif_is_identical(tagvals[0], ATOM_ABBREV) && enif_get_int(env, tagvals[1], &n) && n > 0 && n <= GIT_OID_SHA1_HEXSIZE) abbrev = n;
        else [[unlikely]]
          return raise_badarg_exception(env, opt);
      }
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
  }

  {
    // git_revwalk_new is documented not to be thread-safe, we need to
    // serialize access
    std::lock_guard<std::mutex> guard(s_walk_mutex);

    SmartPtr<git_revwalk> walk(git_revwalk_free);
    if (git_revwalk_new(&walk, repo) != GIT_OK) [[unlikely]]
      return make_git_error(env, "Allocating revwalk");

    git_revwalk_sorting(walk, sort);

    auto res = revwalk_parse_revs(env, repo, walk, revspecs, limit);

    if (res != 0) [[unlikely]]
      return res;

    std::vector<ERL_NIF_TERM> out;
    git_oid oid;
    int i = 0;
    while (!git_revwalk_next(&oid, walk) && i++ < limit) {
      char buf[GIT_OID_SHA1_HEXSIZE+1];
      git_oid_fmt(buf, &oid);
      buf[abbrev] = '\0';
      out.push_back(make_binary(env, buf));
    }

    return enif_make_list_from_array(env, &out.front(), out.size());
  }
}
