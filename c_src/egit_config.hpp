//-----------------------------------------------------------------------------
// libgit2 "config" - perform git get/set config
//-----------------------------------------------------------------------------
#pragma once

#include <git2/common.h>

ERL_NIF_TERM lg2_config(ErlNifEnv* env, ERL_NIF_TERM src, ERL_NIF_TERM kk, ERL_NIF_TERM vv)
{
  git_config_level_t level = git_config_level_t(GIT_CONFIG_HIGHEST_LEVEL-1);
  GitRepoPtr*        repo  = nullptr;

  if      (enif_is_identical(ATOM_DEFAULT, src)) level = git_config_level_t(0);
  else if (enif_is_identical(ATOM_SYSTEM,  src)) level = GIT_CONFIG_LEVEL_SYSTEM;
  else if (enif_is_identical(ATOM_XDG,     src)) level = GIT_CONFIG_LEVEL_XDG;
  else if (enif_is_identical(ATOM_GLOBAL,  src)) level = GIT_CONFIG_LEVEL_GLOBAL;
  else if (enif_is_identical(ATOM_LOCAL,   src)) level = GIT_CONFIG_LEVEL_LOCAL;
  else if (enif_is_identical(ATOM_APP,     src)) level = GIT_CONFIG_LEVEL_APP;
  else if (enif_is_identical(ATOM_HIGHEST, src)) level = GIT_CONFIG_HIGHEST_LEVEL;
  else if (!enif_get_resource(env, src, GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  if (level == git_config_level_t(GIT_CONFIG_HIGHEST_LEVEL-1) && !repo) [[unlikely]]
    return raise_badarg_exception(env, src);

  ErlNifBinary key, val;

  if (!enif_inspect_binary(env, kk, &key) || key.size == 0) [[unlikely]]
    return raise_badarg_exception(env, kk);

  if (vv && (!enif_inspect_binary(env, vv, &val) || val.size == 0)) [[unlikely]]
    return raise_badarg_exception(env, vv);

  SmartPtr<git_config> cfg(git_config_free);

  if (level >= GIT_CONFIG_HIGHEST_LEVEL) {
    SmartPtr<git_config> def_cfg(git_config_free);
    auto err = git_config_open_default(&def_cfg);
    if (err != GIT_OK)
      return make_git_error(env, "Unable to open default configuration");
    else if (level == git_config_level_t(0))
      cfg.swap(def_cfg);
    else if (git_config_open_level(&cfg, def_cfg, level) != GIT_OK)
      return make_git_error(env, std::format("Unable to open config level {}", atom_to_str(env, src)));

    assert(cfg.get());
  } else {
    assert(repo);
    if (git_repository_config(&cfg, repo->get()) < 0) [[unlikely]]
      return make_git_error(env, "Unable to obtain repository config");
  }

  std::string k = bin_to_str(key);

  if (vv) {
    std::string v = bin_to_str(val);

    return git_config_set_string(cfg, k.c_str(), v.c_str()) < 0
      ? make_git_error(env, std::format("Unable to set configuration {} = {}", k, v))
      : ATOM_OK;
  } else {
    SmartPtr<git_config_entry> entry(git_config_entry_free);
    auto err = git_config_get_entry(&entry, cfg, k.c_str());

   if (err == GIT_ENOTFOUND)
      return make_error(env, ATOM_NOT_FOUND);
    if (err < 0) [[unlikely]]
      return make_git_error(env, std::format("Unable to get configuration for {}", k));

    return enif_make_tuple2(env, ATOM_OK, make_binary(env, entry->value));
  }
}