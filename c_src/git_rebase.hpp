//-----------------------------------------------------------------------------
// libgit2 "rebase" - rebase commits onto another branch
//-----------------------------------------------------------------------------
#pragma once

#include <git2/rebase.h>

ERL_NIF_TERM lg2_rebase_init(ErlNifEnv* env, git_repository* repo, std::string const& onto_ref)
{
  SmartPtr<git_reference> ref(git_reference_free);
  if (git_reference_lookup(&ref, repo, onto_ref.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to lookup onto reference {}", onto_ref));

  SmartPtr<git_annotated_commit> onto(git_annotated_commit_free);
  if (git_annotated_commit_from_ref(&onto, repo, ref) != GIT_OK)
    return make_git_error(env, "Failed to create annotated commit");

  SmartPtr<git_rebase> rebase(git_rebase_free);
  git_rebase_options rebase_opts = GIT_REBASE_OPTIONS_INIT;

  if (git_rebase_init(&rebase, repo, nullptr, onto, nullptr, &rebase_opts) != GIT_OK)
    return make_git_error(env, "Failed to initialize rebase");

  return enif_make_int64(env, git_rebase_operation_entrycount(rebase));
}

ERL_NIF_TERM lg2_rebase_next(ErlNifEnv* env, git_repository* repo)
{
  SmartPtr<git_rebase> rebase(git_rebase_free);

  if (git_rebase_open(&rebase, repo, nullptr) != GIT_OK)
    return make_git_error(env, "Failed to open rebase");

  git_rebase_operation* operation = nullptr;
  int result = git_rebase_next(&operation, rebase);

  if (result == GIT_ITEROVER) {
    return ATOM_DONE;
  } else if (result != GIT_OK) {
    return make_git_error(env, "Failed to get next rebase operation");
  }

  if (!operation) {
    return ATOM_DONE;
  }

  auto op_info = enif_make_tuple2(env,
    enif_make_int64(env, operation->type),
    make_binary(env, std::string_view((const char*)operation->exec, GIT_OID_SHA1_HEXSIZE))
  );

  return op_info;
}

ERL_NIF_TERM lg2_rebase_finish(ErlNifEnv* env, git_repository* repo)
{
  SmartPtr<git_rebase> rebase(git_rebase_free);

  if (git_rebase_open(&rebase, repo, nullptr) != GIT_OK)
    return make_git_error(env, "Failed to open rebase");

  if (git_rebase_finish(rebase, nullptr) != GIT_OK)
    return make_git_error(env, "Failed to finish rebase");

  return ATOM_OK;
}

ERL_NIF_TERM lg2_rebase_abort(ErlNifEnv* env, git_repository* repo)
{
  SmartPtr<git_rebase> rebase(git_rebase_free);

  if (git_rebase_open(&rebase, repo, nullptr) != GIT_OK)
    return make_git_error(env, "Failed to open rebase");

  if (git_rebase_abort(rebase) != GIT_OK)
    return make_git_error(env, "Failed to abort rebase");

  return ATOM_OK;
}
