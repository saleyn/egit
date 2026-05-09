//-----------------------------------------------------------------------------
// libgit2 "revert" - revert commits
//-----------------------------------------------------------------------------
#pragma once

#include <git2/revert.h>

ERL_NIF_TERM lg2_revert(ErlNifEnv* env, git_repository* repo, std::string const& commit_oid_str)
{
  git_oid commit_oid;
  if (git_oid_fromstr(&commit_oid, commit_oid_str.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Invalid commit OID: {}", commit_oid_str));

  SmartPtr<git_commit> commit(git_commit_free);
  if (git_commit_lookup(&commit, repo, &commit_oid) != GIT_OK)
    return make_git_error(env, std::format("Failed to lookup commit {}", commit_oid_str));

  git_revert_options options = GIT_REVERT_OPTIONS_INIT;

  int result = git_revert(repo, commit, &options);

  switch (result) {
    case GIT_OK: return ATOM_OK;
    case GIT_EMERGECONFLICT: {
      SmartPtr<git_index> index(git_index_free);
      if (git_repository_index(&index, repo) == GIT_OK) {
        std::vector<ERL_NIF_TERM> conflicts;

        for (size_t i = 0; i < git_index_entrycount(index); ++i) {
          auto entry = git_index_get_byindex(index, i);
          if (entry) {
            unsigned int stage = GIT_INDEX_ENTRY_STAGE(entry);
            if (stage != 0) {
              conflicts.push_back(make_binary(env, std::string_view(entry->path)));
            }
          }
        }

        if (!conflicts.empty()) {
          auto conflict_list = make_list(env, conflicts);
          return enif_make_tuple2(env, ATOM_CONFLICT, conflict_list);
        }
      }
      return make_git_error(env, "Revert conflict occurred");
    }
    default:
      return make_git_error(env, "Failed to revert commit");
  }
}
