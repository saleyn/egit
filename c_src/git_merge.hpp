//-----------------------------------------------------------------------------
// libgit2 "merge" - merge branches together
//-----------------------------------------------------------------------------
#pragma once

#include <git2/merge.h>

ERL_NIF_TERM lg2_merge(ErlNifEnv* env, git_repository* repo, std::string const& branch_oid_str)
{
  git_oid branch_oid;
  if (git_oid_fromstr(&branch_oid, branch_oid_str.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Invalid branch OID: {}", branch_oid_str));

  SmartPtr<git_annotated_commit> their_head(git_annotated_commit_free);
  if (git_annotated_commit_lookup(&their_head, repo, &branch_oid) != GIT_OK)
    return make_git_error(env, std::format("Failed to lookup annotated commit {}", branch_oid_str));

  git_merge_options    merge_opts    = GIT_MERGE_OPTIONS_INIT;
  git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
  checkout_opts.checkout_strategy    = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_RECREATE_MISSING;

  git_merge_analysis_t merge_analysis;
  git_merge_preference_t merge_preference;
  if (git_merge_analysis(&merge_analysis, &merge_preference, repo, (const git_annotated_commit**)&their_head, 1) != GIT_OK)
    return make_git_error(env, "Failed to analyze merge");

  if (merge_analysis & GIT_MERGE_ANALYSIS_UP_TO_DATE) {
    return enif_make_tuple2(env, ATOM_OK, ATOM_UP_TO_DATE);
  }

  if (merge_analysis & GIT_MERGE_ANALYSIS_FASTFORWARD) {
    SmartPtr<git_object> target(git_object_free);
    if (git_revparse_single(&target, repo, branch_oid_str.c_str()) != GIT_OK)
      return make_git_error(env, "Failed to lookup branch");

    SmartPtr<git_reference> ref(git_reference_free);
    if (git_repository_head(&ref, repo) != GIT_OK)
      return make_git_error(env, "Failed to get HEAD");

    SmartPtr<git_reference> new_ref(git_reference_free);
    if (git_reference_set_target(&new_ref, ref, &branch_oid, "merge: fast-forward") != GIT_OK)
      return make_git_error(env, "Failed to fast-forward");

    return enif_make_tuple2(env, ATOM_OK, ATOM_FAST_FORWARD);
  }

  if (git_merge(repo, (const git_annotated_commit**)&their_head, 1, &merge_opts, &checkout_opts) != GIT_OK)
    return make_git_error(env, "Merge failed");

  SmartPtr<git_index> index(git_index_free);
  if (git_repository_index(&index, repo) != GIT_OK)
    return make_git_error(env, "Failed to get index");

  git_index_conflict_iterator* conflict_iter = nullptr;
  if (git_index_conflict_iterator_new(&conflict_iter, index) == GIT_OK) {
    const git_index_entry *ancestor, *ours, *theirs;
    std::vector<ERL_NIF_TERM> conflicts;

    while (git_index_conflict_next(&ancestor, &ours, &theirs, conflict_iter) == 0) {
      if (ours) {
        conflicts.push_back(make_binary(env, std::string_view(ours->path)));
      }
    }

    git_index_conflict_iterator_free(conflict_iter);

    if (!conflicts.empty()) {
      auto conflict_list = make_list(env, conflicts);
      return enif_make_tuple2(env, ATOM_CONFLICT, conflict_list);
    }
  }

  return enif_make_tuple2(env, ATOM_OK, ATOM_MERGED);
}
