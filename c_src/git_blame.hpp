//-----------------------------------------------------------------------------
// libgit2 "blame" - show file change history by line
//-----------------------------------------------------------------------------
#pragma once

#include <git2/blame.h>

ERL_NIF_TERM lg2_blame(ErlNifEnv* env, git_repository* repo, std::string const& path, ERL_NIF_TERM opts)
{
  SmartPtr<git_blame> blame(git_blame_free);
  git_blame_options blame_opts = GIT_BLAME_OPTIONS_INIT;

  if (git_blame_file(&blame, repo, path.c_str(), &blame_opts) != GIT_OK)
    return make_git_error(env, std::format("Failed to get blame for {}", path));

  size_t hunk_count = git_blame_get_hunk_count(blame);
  std::vector<ERL_NIF_TERM> lines;

  for (size_t i = 0; i < hunk_count; ++i) {
    auto hunk = git_blame_get_hunk_byindex(blame, i);
    if (!hunk) continue;

    auto sig = hunk->final_signature;
    auto author_tuple = enif_make_tuple2(env, make_binary(env, sig->name), make_binary(env, sig->email));

    for (size_t j = 0; j < hunk->lines_in_hunk; ++j) {
      size_t line_num = hunk->final_start_line_number + j;
      auto line_info = enif_make_tuple4(env,
        enif_make_uint64(env, line_num),
        author_tuple,
        oid_to_bin_term(env, &hunk->final_commit_id),
        enif_make_int64(env, sig->when.time)
      );
      lines.push_back(line_info);
    }
  }

  return make_list(env, lines);
}
