//-----------------------------------------------------------------------------
// libgit2 "describe" - describe working tree state
//-----------------------------------------------------------------------------
#pragma once

#include <git2/describe.h>

ERL_NIF_TERM lg2_describe(ErlNifEnv* env, git_repository* repo, std::string const& rev, ERL_NIF_TERM opts)
{
  git_describe_options desc_opts = GIT_DESCRIBE_OPTIONS_INIT;
  ERL_NIF_TERM opt;

  while (enif_get_list_cell(env, opts, &opt, &opts)) {
    if (enif_is_tuple(env, opt)) {
      int arity;
      const ERL_NIF_TERM* tuple;
      if (!enif_get_tuple(env, opt, &arity, &tuple) || arity != 2)
        continue;

      if (enif_is_identical(tuple[0], ATOM_PATTERN)) {
        ErlNifBinary bin;
        if (enif_inspect_binary(env, tuple[1], &bin))
          desc_opts.pattern = strndup((const char*)bin.data, bin.size);
      }
    }
  }

  SmartPtr<git_object> obj(git_object_free);
  if (git_revparse_single(&obj, repo, rev.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to parse revision {}", rev));

  SmartPtr<git_describe_result> describe_result(git_describe_result_free);
  if (git_describe_commit(&describe_result, obj, &desc_opts) != GIT_OK)
    return make_git_error(env, "Failed to describe commit");

  SmartPtr<git_buf> buf{[](git_buf* b) { git_buf_dispose(b); }};
  git_buf output = {nullptr, 0, 0};

  if (git_describe_format(&output, describe_result, 0) != GIT_OK)
    return make_git_error(env, "Failed to format describe result");

  auto result = make_binary(env, std::string_view(output.ptr, output.size));
  git_buf_dispose(&output);

  if (desc_opts.pattern)
    free((void*)desc_opts.pattern);

  return enif_make_tuple2(env, ATOM_OK, result);
}
