//-----------------------------------------------------------------------------
// libgit2 "diff" - compare revisions or working directory with index
//-----------------------------------------------------------------------------
#pragma once

#include <git2/diff.h>

ERL_NIF_TERM lg2_diff(ErlNifEnv* env, git_repository* repo, std::string const& from_rev, std::string const& to_rev, ERL_NIF_TERM opts)
{
  SmartPtr<git_object> from_obj(git_object_free);
  SmartPtr<git_object> to_obj(git_object_free);
  SmartPtr<git_object> from_peeled(git_object_free);
  SmartPtr<git_object> to_peeled(git_object_free);

  if (git_revparse_single(&from_obj, repo, from_rev.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to parse revision {}", from_rev));

  if (git_revparse_single(&to_obj, repo, to_rev.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to parse revision {}", to_rev));

  if (git_object_peel(&from_peeled, from_obj, GIT_OBJ_TREE) != GIT_OK)
    return make_git_error(env, "Failed to get tree from source revision");

  if (git_object_peel(&to_peeled, to_obj, GIT_OBJ_TREE) != GIT_OK)
    return make_git_error(env, "Failed to get tree from target revision");

  SmartPtr<git_diff> diff(git_diff_free);
  git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;

  if (git_diff_tree_to_tree(&diff, repo, (git_tree*)from_peeled.get(), (git_tree*)to_peeled.get(), &diff_opts) != GIT_OK)
    return make_git_error(env, "Failed to compute diff");

  std::vector<ERL_NIF_TERM> deltas;
  size_t num_deltas = git_diff_num_deltas(diff);

  for (size_t i = 0; i < num_deltas; ++i) {
    auto delta = git_diff_get_delta(diff, i);
    if (!delta) continue;

    const char* status_str = "";
    switch (delta->status) {
      case GIT_DELTA_ADDED:      status_str = "added";      break;
      case GIT_DELTA_DELETED:    status_str = "deleted";    break;
      case GIT_DELTA_MODIFIED:   status_str = "modified";   break;
      case GIT_DELTA_RENAMED:    status_str = "renamed";    break;
      case GIT_DELTA_COPIED:     status_str = "copied";     break;
      case GIT_DELTA_TYPECHANGE: status_str = "typechange"; break;
      default:                   status_str = "unknown";    break;
    }

    auto delta_info = enif_make_tuple4(env,
      make_binary(env, std::string_view(delta->new_file.path)),
      make_binary(env, status_str),
      enif_make_int64(env, delta->status),
      enif_make_int64(env, delta->similarity)
    );
    deltas.push_back(delta_info);
  }

  return make_list(env, deltas);
}
