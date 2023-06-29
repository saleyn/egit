//-----------------------------------------------------------------------------
// libgit2 "remote" - add/rename/delete/set-url/list remotes
//-----------------------------------------------------------------------------
#pragma once

#include <git2/common.h>

namespace {
  enum class RemoteOp {
    UNDEFINED,
    ADD,
    RENAME,
    DELETE,
    SETURL,
  };
}

ERL_NIF_TERM lg2_remote(
  ErlNifEnv* env, git_repository* repo, std::string const& name, ERL_NIF_TERM op, ERL_NIF_TERM opts)
{
  int                     arity;
  ERL_NIF_TERM            opt;
  const ERL_NIF_TERM*     tagvals;
  ErlNifBinary            bin;
  RemoteOp                kind = RemoteOp::UNDEFINED;
  auto                    push = false;
  std::string             val;

  // Parse options
  if      (enif_is_identical(op, ATOM_DELETE))
    kind = RemoteOp::DELETE;
  else if (enif_get_tuple(env, op, &arity, &tagvals) && arity == 2) {
    if (enif_is_identical(tagvals[0], ATOM_ADD) && enif_inspect_binary(env, tagvals[1], &bin)) {
      kind = RemoteOp::ADD;
      val  = bin_to_str(bin);
    } else if (enif_is_identical(tagvals[0], ATOM_RENAME) && enif_inspect_binary(env, tagvals[1], &bin)) {
      kind = RemoteOp::RENAME;
      val  = bin_to_str(bin);
    } else if (enif_is_identical(tagvals[0], ATOM_SETURL) && enif_inspect_binary(env, tagvals[1], &bin)) {
      kind = RemoteOp::SETURL;
      val  = bin_to_str(bin);
      while (enif_get_list_cell(env, opts, &opt, &opts)) {
        if (enif_is_identical(opt, ATOM_PUSH)) push = true;
        else [[unlikely]]
          goto ERR;
      }
    } else [[unlikely]]
      goto ERR;
  }
  else [[unlikely]]
    goto ERR;


  switch (kind) {
    case RemoteOp::ADD: {
      git_remote* remote;
      return git_remote_create(&remote, repo, name.c_str(), val.c_str()) == GIT_OK
           ? ATOM_OK : make_git_error(env, "Could not create remote");
    }
    case RemoteOp::DELETE:
      return git_remote_delete(repo, name.c_str()) == GIT_OK
           ? ATOM_OK : make_git_error(env, "Could not delete remote");
    case RemoteOp::SETURL:
      return (push ? git_remote_set_pushurl(repo, name.c_str(), val.c_str())
                   : git_remote_set_url(repo, name.c_str(), val.c_str())) == GIT_OK
           ? ATOM_OK : make_git_error(env, "Could not set-url");
    case RemoteOp::RENAME: {
      GitStrArray problems;
      if (git_remote_rename(&problems, repo, name.c_str(), val.c_str()) == GIT_OK) [[likely]]
        return ATOM_OK;

      if (!problems.size())
        return make_git_error(env, "Could not rename remote");

      std::vector<ERL_NIF_TERM> errs;
      errs.reserve(problems.size());
      for (auto i=0u; i < problems.size(); ++i)
        errs.push_back(make_binary(env, problems[i]));

      return make_error(env, enif_make_list_from_array(env, &errs.front(), errs.size()));
    }
    default:
      break;
  }
ERR:
  return raise_badarg_exception(env, op);
}

ERL_NIF_TERM lg2_remotes_list(ErlNifEnv* env, git_repository* repo)
{
  GitStrArray remotes;

  if (git_remote_list(&remotes, repo) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not retrieve remotes");

  std::vector<ERL_NIF_TERM> res;
  res.reserve(remotes.size()*2);

  for (auto i = 0u; i < remotes.size(); i++) {
    auto name = remotes[i];

    SmartPtr<git_remote> remote(git_remote_free);
    if (git_remote_lookup(&remote, repo, name) != GIT_OK)
      return make_git_error(env, std::format("Could not look up remote {}", name));

    auto fetch  = git_remote_url(remote);
    auto push   = git_remote_pushurl(remote);
    auto single = fetch && (!push || strcmp(fetch, push) == 0);

    ERL_NIF_TERM ll[] = {ATOM_PUSH, ATOM_FETCH};

    if (push && !single)
      res.push_back(
        enif_make_tuple3(env,
          make_binary(env, name),
          make_binary(env, push),
          enif_make_list_from_array(env, ll, 1)));

    if (fetch) {
      ERL_NIF_TERM type = single
        ? enif_make_list_from_array(env, ll, 2)
        : enif_make_list_from_array(env, ll+1, 1);
      res.push_back(enif_make_tuple3(env, make_binary(env, name), make_binary(env, fetch), type));
    }
  }

  return enif_make_list_from_array(env, &res.front(), res.size());
}
