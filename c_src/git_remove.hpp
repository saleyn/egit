//-----------------------------------------------------------------------------
// libgit2 "remove" and "move" - remove and rename tracked files
//-----------------------------------------------------------------------------
#pragma once

ERL_NIF_TERM lg2_remove(ErlNifEnv* env, git_repository* repo, std::string const& path)
{
  SmartPtr<git_index> index(git_index_free);
  if (git_repository_index(&index, repo) != GIT_OK)
    return make_git_error(env, "Failed to get repository index");

  if (git_index_remove(index, path.c_str(), 0) != GIT_OK)
    return make_git_error(env, std::format("Failed to remove {} from index", path));

  if (git_index_write(index) != GIT_OK)
    return make_git_error(env, "Failed to write index");

  return ATOM_OK;
}

ERL_NIF_TERM lg2_move(ErlNifEnv* env, git_repository* repo, std::string const& old_path, std::string const& new_path)
{
  SmartPtr<git_index> index(git_index_free);
  if (git_repository_index(&index, repo) != GIT_OK)
    return make_git_error(env, "Failed to get repository index");

  if (git_index_remove(index, old_path.c_str(), 0) != GIT_OK)
    return make_git_error(env, std::format("Failed to remove {} from index", old_path));

  if (git_index_add_bypath(index, new_path.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to add {} to index", new_path));

  if (git_index_write(index) != GIT_OK)
    return make_git_error(env, "Failed to write index");

  return ATOM_OK;
}
