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

ERL_NIF_TERM lg2_rev_parse(ErlNifEnv* env, git_repository* repo, std::string const& spec)
{
  git_revspec rs;

  if (git_revparse(&rs, repo, spec.c_str()) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not parse");

  std::vector<ERL_NIF_TERM> keys;
  std::vector<ERL_NIF_TERM> vals;

  if ((rs.flags & GIT_REVSPEC_SINGLE) != 0) {
    keys.push_back(ATOM_OID);
    vals.push_back(make_binary(env, oid_to_str(git_object_id(rs.from))));
    git_object_free(rs.from);
  }
  else if ((rs.flags & GIT_REVSPEC_RANGE) != 0) {
    keys.push_back(ATOM_TO);
    vals.push_back(make_binary(env, oid_to_str(git_object_id(rs.to))));
    git_object_free(rs.to);

    if ((rs.flags & GIT_REVSPEC_MERGE_BASE) != 0) {
      git_oid base;
      if (git_merge_base(&base, repo, git_object_id(rs.from), git_object_id(rs.to)) != GIT_OK)
        return make_git_error(env, "Could not find merge base " + spec);

      keys.push_back(ATOM_MERGE_BASE);
      vals.push_back(make_binary(env, oid_to_str(base)));
    }

    keys.push_back(ATOM_FROM);
    vals.push_back(make_binary(env, oid_to_str(git_object_id(rs.from))));
    git_object_free(rs.from);
  }
  else
    return make_git_error(env, "Invalid results from git_revparse " + spec);

  ERL_NIF_TERM map;
  return enif_make_map_from_arrays(env, &keys.front(), &vals.front(), keys.size(), &map)
       ? map : make_error(env, ATOM_ENOMEM);
}
