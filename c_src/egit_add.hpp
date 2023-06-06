//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/add.c
//-----------------------------------------------------------------------------
// libgit2 "add" - modify the index
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

enum index_mode {
  INDEX_NONE,
  INDEX_ADD
};

struct index_options {
  index_options()
    : dry_run(false), verbose(false), update(false), force(false)
    , repo(nullptr),  mode(INDEX_ADD)
  {}

  bool            dry_run;
  bool            verbose;
  bool            update;
  bool            force;
  git_repository* repo;
  index_mode      mode;
  ErlNifEnv*      env;
  std::vector<ERL_NIF_TERM> files;
};

/* Forward declarations for helpers */
static ERL_NIF_TERM
parse_opts(ErlNifEnv* env, std::vector<std::string>& file_specs, ERL_NIF_TERM file_specs_list, ERL_NIF_TERM opts, index_options& o)
{
  ERL_NIF_TERM spec, opt;

  file_specs.clear();

  while (enif_get_list_cell(env, file_specs_list, &spec, &file_specs_list)) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, spec, &bin)) [[unlikely]]
      return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, spec));

    file_specs.emplace_back(std::string((const char*)bin.data, bin.size));
  }

  while (enif_get_list_cell(env, opts, &opt, &opts)) {
    if      (enif_is_identical(opt, ATOM_VERBOSE)) o.verbose = true;
    else if (enif_is_identical(opt, ATOM_DRY_RUN)) o.dry_run = true;
    else if (enif_is_identical(opt, ATOM_UPDATE))  o.update  = true;
    else if (enif_is_identical(opt, ATOM_FORCE))   o.force   = true;
    else [[unlikely]]
      return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, opt));
  }

  return 0;
}

// This callback is called for each file under consideration by
// git_index_(update|add)_all.
// It makes use of the callback's ability to abort the action.
int visit_matched_cb(const char* path, [[maybe_unused]] const char* matched_pathspec, void* payload)
{
  auto opts = static_cast<index_options*>(payload);
  unsigned status;

  /* Get the file status */
  if (git_status_file(&status, opts->repo, path) < 0)
    return -1; // Abort

  auto should_add = (status & GIT_STATUS_WT_MODIFIED) || (status & GIT_STATUS_WT_NEW);
  int  ret        = opts->dry_run || !should_add; // Skip = 1, Add = 0

  if (should_add)
    opts->files.push_back(make_binary(opts->env, path));

  return ret;
}

// Implementation of "add" logic
ERL_NIF_TERM lg2_add(ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM file_specs_list, ERL_NIF_TERM opts)
{
  assert(repo);

  git_index_matched_path_cb matched_cb = NULL;
  index_options             options;
  std::vector<std::string>  path_specs;
  git_strarray              array{};

  options.env  = env;
  options.repo = repo;
  options.mode = INDEX_ADD;

  // Parse the options & arguments
  auto res = parse_opts(env, path_specs, file_specs_list, opts, options);

  if (res != 0) [[unlikely]]
    return res;

  SmartPtr<git_index> index(git_index_free);

  // Grab the repository's index
  if (git_repository_index(&index, repo) != GIT_OK)
    return make_git_error(env, "Could not open repository index");

  // Setup a callback if the requested options need it
  if (options.verbose || options.dry_run)
    matched_cb = &visit_matched_cb;

  array.count = path_specs.size();
  std::vector<const char*> strings(array.count);

  for (auto i = 0u; i < array.count; ++i)
    strings[i] = path_specs[i].c_str();

  array.strings = const_cast<char**>(&strings[0]);
  auto flags    = options.force ? GIT_INDEX_ADD_FORCE : 0;

  // Perform the requested action with the index and files
  auto err = options.update
           ? git_index_update_all(index, &array, matched_cb, &options)
           : git_index_add_all(index, &array, flags, matched_cb, &options);

  // Cleanup memory
  if (err != GIT_OK)
    return err;

  if (options.files.size())
    git_index_write(index);

  auto files = enif_make_list_from_array(env, &options.files.front(), options.files.size());
  auto mode  = options.files.empty() ? ATOM_NONE : (options.dry_run ? ATOM_DRY_RUN : ATOM_ADDED);

  ERL_NIF_TERM keys[] = {ATOM_MODE, ATOM_FILES};
  ERL_NIF_TERM vals[] = {mode,      files};

  ERL_NIF_TERM map;
  return enif_make_map_from_arrays(env, keys, vals, 2, &map) ? map : make_error(env, ATOM_ENOMEM);
}
