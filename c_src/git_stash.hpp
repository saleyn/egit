//-----------------------------------------------------------------------------
// libgit2 "stash" - temporarily save uncommitted changes
//-----------------------------------------------------------------------------
#pragma once

#include <git2/stash.h>

ERL_NIF_TERM lg2_stash_save(ErlNifEnv* env, git_repository* repo, std::string const& message)
{
  git_oid stash_oid;
  git_signature *sig = nullptr;

  if (git_signature_now(&sig, "egit", "egit@example.com") != GIT_OK)
    return make_git_error(env, "Failed to create signature");

  ScopeCleanup cleanup([sig]() { git_signature_free(sig); });

  int result = git_stash_save(&stash_oid, repo, sig, message.c_str(),
                               GIT_STASH_DEFAULT);

  if (result != GIT_OK && result != GIT_ENOTFOUND)
    return make_git_error(env, "Failed to save stash");

  if (result == GIT_ENOTFOUND)
    return make_error(env, ATOM_NO_CHANGES);

  return enif_make_tuple2(env, ATOM_OK, oid_to_bin_term(env, &stash_oid));
}

ERL_NIF_TERM lg2_stash_list(ErlNifEnv* env, git_repository* repo)
{
  std::vector<ERL_NIF_TERM> stashes;

  auto stash_cb = [](size_t index, const char* name, const git_oid* oid, void* payload) -> int {
    auto* env = (ErlNifEnv*)payload;
    auto* stashes = (std::vector<ERL_NIF_TERM>*)payload;

    if (!stashes) return 0;

    auto entry = enif_make_tuple2(env,
      enif_make_int64(env, index),
      make_binary(env, std::string_view(name))
    );
    stashes->push_back(entry);
    return 0;
  };

  git_stash_foreach(repo, stash_cb, &stashes);

  return make_list(env, stashes);
}

ERL_NIF_TERM lg2_stash_apply(ErlNifEnv* env, git_repository* repo, size_t stash_index)
{
  git_stash_apply_options options = GIT_STASH_APPLY_OPTIONS_INIT;

  int result = git_stash_apply(repo, stash_index, &options);

  switch (result) {
    case GIT_OK:
      return ATOM_OK;
    case GIT_EMERGECONFLICT: {
      SmartPtr<git_index> index(git_index_free);
      if (git_repository_index(&index, repo) == GIT_OK) {
        std::vector<ERL_NIF_TERM> conflicts;

        for (size_t i = 0; i < git_index_entrycount(index); ++i) {
          auto entry = git_index_get_byindex(index, i);
          if (entry) {
            unsigned int stage = GIT_INDEX_ENTRY_STAGE(entry);
            if (stage != 0)
              conflicts.push_back(make_binary(env, std::string_view(entry->path)));
          }
        }

        if (!conflicts.empty()) {
          auto conflict_list = make_list(env, conflicts);
          return enif_make_tuple2(env, ATOM_CONFLICT, conflict_list);
        }
      }
      return make_git_error(env, "Stash apply conflict occurred");
    }
    default:
      return make_git_error(env, "Failed to apply stash");
  }
}

ERL_NIF_TERM lg2_stash_pop(ErlNifEnv* env, git_repository* repo, size_t stash_index)
{
  git_stash_apply_options options = GIT_STASH_APPLY_OPTIONS_INIT;

  int result = git_stash_pop(repo, stash_index, &options);

  switch (result) {
    case GIT_OK:
      return ATOM_OK;
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
      return make_git_error(env, "Stash pop conflict occurred");
    }
    default:
      return make_git_error(env, "Failed to pop stash");
  }
}

ERL_NIF_TERM lg2_stash_drop(ErlNifEnv* env, git_repository* repo, size_t stash_index)
{
  if (git_stash_drop(repo, stash_index) != GIT_OK)
    return make_git_error(env, "Failed to drop stash");

  return ATOM_OK;
}
