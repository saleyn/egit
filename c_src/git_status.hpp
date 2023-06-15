//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/status.c
//-----------------------------------------------------------------------------
// libgit2 "status" - get the repository status
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

namespace {
  struct VisitorState {
    VisitorState(std::vector<ERL_NIF_TERM>& submods, ErlNifEnv* env)
      : submods(submods), env(env)
    {}

    std::vector<ERL_NIF_TERM>& submods;
    ErlNifEnv*                env;
  };
}

ERL_NIF_TERM lg2_status(ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM opts)
{
  assert(repo);

  git_status_options statusopt = GIT_STATUS_OPTIONS_INIT;

  statusopt.show  = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
  statusopt.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED     |
                    GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX |
                    GIT_STATUS_OPT_SORT_CASE_SENSITIVELY;

  std::vector<std::string> paths;
  auto                     showbranch = false;
  auto                     showsubmod = false;

  if (git_repository_is_bare(repo)) [[unlikely]]
    return make_error(env,
      std::format("Cannot report status on bare repository {}", git_repository_path(repo)));

  //---- Parse args
  for (ERL_NIF_TERM opt; enif_get_list_cell(env, opts, &opt, &opts);) {
    int arity;
    const ERL_NIF_TERM* tagvals;

    if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
      ERL_NIF_TERM val;
      if (parse_atom_if(env, ATOM_UNTRACKED, tagvals, val)) [[likely]] {
        if      (enif_is_identical(ATOM_NONE,      val)) statusopt.flags &= ~GIT_STATUS_OPT_INCLUDE_UNTRACKED;
        else if (enif_is_identical(ATOM_NORMAL,    val)) statusopt.flags |= GIT_STATUS_OPT_INCLUDE_UNTRACKED;
        else if (enif_is_identical(ATOM_RECURSIVE, val)) statusopt.flags |= GIT_STATUS_OPT_INCLUDE_UNTRACKED |
                                                                            GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;
        else
          return raise_badarg_exception(env, opt);
      }
      else if (enif_is_identical(ATOM_PATHS, tagvals[0]) && enif_is_list(env, tagvals[1])) {
        for (ERL_NIF_TERM o, list=tagvals[1]; enif_get_list_cell(env, list, &o, &list);) {
          std::string tmp;
          if (!term_to_str(env, o, tmp))
            return raise_badarg_exception(env, opt);
          paths.push_back(tmp);
        }
      }
      else
        return raise_badarg_exception(env, opt);

    }
    else if (enif_is_identical(ATOM_BRANCH,            opt)) showbranch       = true;
    else if (enif_is_identical(ATOM_IGNORED,           opt)) statusopt.flags |= GIT_STATUS_OPT_INCLUDE_IGNORED;
    else if (enif_is_identical(ATOM_IGNORE_SUBMODULES, opt)) statusopt.flags |= GIT_STATUS_OPT_EXCLUDE_SUBMODULES;
    else if (enif_is_identical(ATOM_SUBMODULES,        opt)) {
      showsubmod = true;
      statusopt.flags &= ~GIT_STATUS_OPT_EXCLUDE_SUBMODULES;
    }
    else [[unlikely]]
      return raise_badarg_exception(env, opt);
  }

  std::vector<const char*> cpaths;
  if (paths.size()) {
    cpaths.reserve(paths.size());
    for (auto& s : paths)
      cpaths.push_back(s.c_str());

    statusopt.pathspec = { .strings = const_cast<char**>(&cpaths.front()), .count = cpaths.size() };
  }

  //---- Process status command

  // We use `git_status_list_new()` to generate a list of status
  // information which lets us iterate over it at our
  // convenience and extract the data we want to show out of
  // each entry.
  //
  // You can use `git_status_foreach()` or
  // `git_status_foreach_ext()` if you'd prefer to execute a
  // callback for each entry. The latter gives you more control
  // about what results are presented.
  SmartPtr<git_status_list> status(git_status_list_free);

  if (git_status_list_new(&status, repo, &statusopt) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not get status");

  std::vector<ERL_NIF_TERM> keys;
  std::vector<ERL_NIF_TERM> vals;

  if (showbranch) {
    SmartPtr<git_reference> head(git_reference_free);

    auto error  = git_repository_head(&head, repo);
    auto branch = error == GIT_OK ? git_reference_shorthand(head) : nullptr;

    if (error < 0 && error != GIT_EUNBORNBRANCH && error != GIT_ENOTFOUND)
      return make_git_error(env, "Failed to get current branch");

    keys.push_back(ATOM_BRANCH);
    vals.push_back(branch ? make_binary(env, branch) : ATOM_NIL);
  }

  if (showsubmod) {
    std::vector<ERL_NIF_TERM> submods;

    auto visit = [](git_submodule* sm, const char*, void* payload)
    {
      VisitorState& state = *static_cast<VisitorState*>(payload);
      auto path = git_submodule_path(sm);
      auto url  = git_submodule_url(sm);
      state.submods.push_back(
        enif_make_tuple2(state.env, make_binary(state.env, path), make_binary(state.env, url)));
      return 0;
    };

    VisitorState state(submods, env);
    if (git_submodule_foreach(repo, visit, &state) != GIT_OK) [[unlikely]]
      return make_git_error(env, "Cannot iterate submodules");

    if (submods.size()) {
      keys.push_back(ATOM_SUBMODULES);
      vals.push_back(enif_make_list_from_array(env, &submods.front(), submods.size()));
    }
  }

  size_t maxi = git_status_list_entrycount(status);

  std::vector<ERL_NIF_TERM> index_changes;
  std::vector<ERL_NIF_TERM> wt_changes;
  std::vector<ERL_NIF_TERM> untracked;
  std::vector<ERL_NIF_TERM> ignored;
  std::vector<ERL_NIF_TERM> submodules;

  // Changes in index
  for (auto i = 0u; i < maxi; ++i) {
    auto s = git_status_byindex(status, i);

    if (s->status == GIT_STATUS_CURRENT)
      continue;

    auto istatus =
      (s->status & GIT_STATUS_INDEX_NEW)        ? ATOM_NEW        :
      (s->status & GIT_STATUS_INDEX_MODIFIED)   ? ATOM_MODIFIED   :
      (s->status & GIT_STATUS_INDEX_DELETED)    ? ATOM_DELETED    :
      (s->status & GIT_STATUS_INDEX_RENAMED)    ? ATOM_RENAMED    :
      (s->status & GIT_STATUS_INDEX_TYPECHANGE) ? ATOM_TYPECHANGE : ATOM_NIL;

    auto wstatus =
      (s->status & GIT_STATUS_WT_MODIFIED)      ? ATOM_MODIFIED   :
      (s->status & GIT_STATUS_WT_DELETED)       ? ATOM_DELETED    :
      (s->status & GIT_STATUS_WT_RENAMED)       ? ATOM_RENAMED    :
      (s->status & GIT_STATUS_WT_TYPECHANGE)    ? ATOM_TYPECHANGE : ATOM_NIL;

    auto idx = s->head_to_index;
    auto wt  = s->index_to_workdir;

    auto fmt_path = [env](auto& v, auto xstatus, auto delta) {
      ERL_NIF_TERM val = ATOM_NIL;
      if (delta->old_file.path && delta->new_file.path) {
        val = strcmp(delta->old_file.path, delta->new_file.path) == 0
            ? enif_make_tuple2(env, xstatus, make_binary(env, delta->old_file.path))
            : enif_make_tuple3(env, xstatus,
                make_binary(env, delta->old_file.path),
                make_binary(env, delta->new_file.path));
      }
      else if (delta->old_file.path && !delta->new_file.path)
        val = enif_make_tuple2(env, xstatus, make_binary(env, delta->old_file.path));
      else if (delta->new_file.path && !delta->old_file.path)
        val = enif_make_tuple2(env, xstatus, make_binary(env, delta->new_file.path));

      if (val != ATOM_NIL)
        v.push_back(val);
    };

    if (idx && istatus != ATOM_NIL)
      fmt_path(index_changes, istatus, idx);

    if (wt && wstatus != ATOM_NIL)
      fmt_path(wt_changes, wstatus, wt);

    if (s->status == GIT_STATUS_WT_NEW && wt->old_file.path)
      untracked.push_back(make_binary(env, wt->old_file.path));

    if (s->status == GIT_STATUS_IGNORED && wt->old_file.path)
      ignored.push_back(make_binary(env, wt->old_file.path));

    // A commit in a tree is how submodules are stored, take a look at its status
    auto smstatus = 0u;
    if (wt && wt->new_file.mode == GIT_FILEMODE_COMMIT &&
        git_submodule_status(&smstatus, repo, wt->new_file.path, GIT_SUBMODULE_IGNORE_UNSPECIFIED) == GIT_OK)
    {
      auto smst =
        (smstatus & GIT_SUBMODULE_STATUS_WD_MODIFIED)       ? ATOM_NEW       :
        (smstatus & GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED) ? ATOM_MODIFIED  :
        (smstatus & GIT_SUBMODULE_STATUS_WD_WD_MODIFIED)    ? ATOM_MODIFIED  :
        (smstatus & GIT_SUBMODULE_STATUS_WD_UNTRACKED)      ? ATOM_UNTRACKED : ATOM_NIL;

      if (smst != ATOM_NIL) {
        submodules.push_back(
          enif_make_tuple2(env, smst, make_binary(env,
              idx && idx->old_file.path ? idx->old_file.path :
              idx && idx->new_file.path ? idx->new_file.path :
              wt  && wt->old_file.path  ? wt->old_file.path  :
              wt  && wt->new_file.path  ? wt->new_file.path  : "")));
      }
    }
  }

  if (index_changes.size()) {
    keys.push_back(ATOM_INDEX);
    vals.push_back(enif_make_list_from_array(env, &index_changes.front(), index_changes.size()));
  }

  if (wt_changes.size()) {
    keys.push_back(ATOM_WORKTREE);
    vals.push_back(enif_make_list_from_array(env, &wt_changes.front(), wt_changes.size()));
  }

  if (untracked.size()) {
    keys.push_back(ATOM_UNTRACKED);
    vals.push_back(enif_make_list_from_array(env, &untracked.front(), untracked.size()));
  }

  if (ignored.size()) {
    keys.push_back(ATOM_IGNORED);
    vals.push_back(enif_make_list_from_array(env, &ignored.front(), ignored.size()));
  }

  if (submodules.size()) {
    keys.push_back(ATOM_SUBMODULES);
    vals.push_back(enif_make_list_from_array(env, &submodules.front(), submodules.size()));
  }

  ERL_NIF_TERM map;
  return enif_make_map_from_arrays(env, &keys.front(), &vals.front(), keys.size(), &map)
       ? map : make_error(env, ATOM_ENOMEM);
}
