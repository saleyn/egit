//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/rev-parse.c
//-----------------------------------------------------------------------------
// libgit2 "rev-parse" - parse revspecs
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

ERL_NIF_TERM lg2_rev_parse(
  ErlNifEnv* env, git_repository* repo, std::string const& spec, ERL_NIF_TERM opts)
{
  int abbrev = GIT_OID_SHA1_HEXSIZE;
  // Parse options
  {
    int arity, n;
    ERL_NIF_TERM opt;
    const ERL_NIF_TERM* tagvals;

    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
        if (enif_is_identical(tagvals[0], ATOM_ABBREV) && enif_get_int(env, tagvals[1], &n) && n > 0 && n <= GIT_OID_SHA1_HEXSIZE)
          abbrev = n;
        else [[unlikely]]
          return raise_badarg_exception(env, opt);
      }
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
  }

  git_revspec rs;

  if (git_revparse(&rs, repo, spec.c_str()) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not parse");

  SmartPtr<git_object> from(git_object_free, rs.from);
  SmartPtr<git_object> to  (git_object_free,   rs.to);

  if ((rs.flags & GIT_REVPARSE_SINGLE) != 0)
    return enif_make_tuple2(env, ATOM_OK, make_binary(env, oid_to_str(rs.from, abbrev)));
  else if ((rs.flags & GIT_REVPARSE_RANGE) == 0) [[unlikely]]
    return make_git_error(env, "Invalid results from git_revparse " + spec);

  std::vector<ERL_NIF_TERM> keys;
  std::vector<ERL_NIF_TERM> vals;

  keys.push_back(ATOM_FROM);
  vals.push_back(make_binary(env, oid_to_str(rs.from, abbrev)));

  keys.push_back(ATOM_TO);
  vals.push_back(make_binary(env, oid_to_str(rs.to, abbrev)));

  if ((rs.flags & GIT_REVPARSE_MERGE_BASE) != 0) {
    git_oid base;
    if (git_merge_base(&base, repo, git_object_id(rs.from), git_object_id(rs.to)) != GIT_OK)
      return make_git_error(env, "Could not find merge base " + spec);

    keys.push_back(ATOM_MERGE_BASE);
    vals.push_back(make_binary(env, oid_to_str(base, abbrev)));
  }

  ERL_NIF_TERM map;
  return enif_make_map_from_arrays(env, &keys.front(), &vals.front(), keys.size(), &map)
       ? map : make_error(env, ATOM_ENOMEM);
}
