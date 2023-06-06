//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/commit.c
//-----------------------------------------------------------------------------
// libgit2 "commit" - commit changes
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

static bool has_changes(git_repository* repo, git_index* index)
{
  auto cb = [](const char*  path,
               unsigned int status_flags,
               void*        payload)
  {
    auto modified = status_flags &
      (
        GIT_STATUS_INDEX_NEW        |
        GIT_STATUS_INDEX_MODIFIED   |
        GIT_STATUS_INDEX_DELETED    |
        GIT_STATUS_INDEX_RENAMED    |
        GIT_STATUS_INDEX_TYPECHANGE
      );

    if (modified) {
      auto* p = static_cast<int*>(payload);
      ++(*p);

      return -1;
    }
    return 1;
  };

  int diff_count = 0;

  git_status_foreach(repo, cb, &diff_count);

  return diff_count;
}

ERL_NIF_TERM lg2_commit(ErlNifEnv* env, git_repository* repo, std::string const& comment)
{
  SmartPtr<git_object> parent(git_object_free);
  SmartPtr<git_commit> parent_commit(git_commit_free);

  auto error = git_revparse_single(&parent, repo, "HEAD");
  if (error == GIT_ENOTFOUND) // HEAD not found. Creating first commit.
    error = 0;
  else if (error != 0) [[unlikely]]
    return make_git_error(env, "Error parsing a revision string");
  else if (git_commit_lookup(&parent_commit, repo, git_object_id(parent))) [[unlikely]]
    return make_git_error(env, "Could not lookup HEAD commit");

  SmartPtr<git_index> index(git_index_free);
  if (git_repository_index(&index, repo) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not open repository index");

  git_oid tree_oid;
  if (git_index_write_tree(&tree_oid, index) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not write tree");

  if (git_index_write(index) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not write index");

  SmartPtr<git_tree> tree(git_tree_free);
  if (git_tree_lookup(&tree, repo, &tree_oid) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Error looking up tree");

  if (!has_changes(repo, index))
    return enif_make_tuple2(env, ATOM_OK, ATOM_NIL);

  SmartPtr<git_signature> signature(git_signature_free);
  if (git_signature_default(&signature, repo) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Error creating signature");

  git_oid commit_oid;

  const git_commit* parents[] = { parent_commit.get() };

  if (git_commit_create(
        &commit_oid, repo, "HEAD", signature, signature, nullptr,
        comment.c_str(), tree, parent ? 1 : 0, parents) != GIT_OK) [[unlikely]]
    return make_git_error(env, git_error_last() ? "" : "Error creating commit");

  return enif_make_tuple2(env, ATOM_OK, make_binary(env, oid_to_str(commit_oid)));
}