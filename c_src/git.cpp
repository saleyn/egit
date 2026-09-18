#include <stdio.h>
#include <cstring>
#include <cassert>
#include <memory>
#include <tuple>
#include <vector>
#include <atomic>

#ifdef HAVE_FORMAT
#include <format>
#else
#include <fmt/format.h>
namespace std { using namespace fmt; }
#endif

#if !defined(GIT_REVSPEC_MERGE_BASE) && defined(GIT_REVPARSE_MERGE_BASE)
#define GIT_REVSPEC_MERGE_BASE GIT_REVPARSE_MERGE_BASE
#endif

#include <git2.h>

#ifndef GIT_OID_SHA1_HEXSIZE
#define GIT_OID_SHA1_HEXSIZE GIT_OID_HEXSZ
#endif

#include "git_utils.hpp"
#include "git_add.hpp"
#include "git_cat_file.hpp"
#include "git_checkout.hpp"
#include "git_commit.hpp"
#include "git_rev_parse.hpp"
#include "git_rev_list.hpp"
#include "git_config.hpp"
#include "git_branch.hpp"
#include "git_index.hpp"
#include "git_remote.hpp"
#include "git_tag.hpp"
#include "git_status.hpp"
#include "git_blame.hpp"
#include "git_describe.hpp"
#include "git_cherry_pick.hpp"
#include "git_reflog.hpp"
#include "git_remove.hpp"
#include "git_diff.hpp"
#include "git_merge.hpp"
#include "git_revert.hpp"
#include "git_rebase.hpp"
#include "git_stash.hpp"
#include "git_credentials.hpp"

static ERL_NIF_TERM to_monitored_resource(ErlNifEnv* env, git_repository* p)
{
  ErlNifMonitor mon;
  ErlNifPid pid;
  enif_self(env, &pid);

  auto rp = GitRepoPtr::create(p);

  if (!rp) [[unlikely]] {
    assert(p);

    #ifdef NIF_DEBUG
    fprintf(stderr, "=egit=> Freeing repo %p [%d]\r\n", p, __LINE__);
    #endif

    git_repository_free(p);
    return enif_raise_exception(env, ATOM_ENOMEM);
  }

  auto result = enif_monitor_process(env, rp, &pid, &mon);

  if (result != 0) [[unlikely]] {
    #ifdef NIF_DEBUG
    fprintf(stderr, "=egit=> Freeing repo %p (result=%d) [%d]\r\n", rp, result, __LINE__);
    #endif
    git_repository_free(p);

    if (result > 0) {
      // Process no longer alive
      return enif_raise_exception(env, ATOM_ENOPROCESS);
    } else {
      assert(result < 0);
      // mon callback is not specified
      return enif_raise_exception(env, ATOM_ENOCALLBACK);
    }
  }

  return rp->to_enif_resource(env);
}

static ERL_NIF_TERM oid_to_bin(ErlNifEnv* env, git_oid const* oid, size_t len = GIT_OID_SHA1_HEXSIZE)
{
  char buf[GIT_OID_SHA1_HEXSIZE+1];
  len = std::min(len, sizeof(buf)-1);
  git_oid_tostr(buf, len, oid);
  return make_binary(env, buf);
}

static ERL_NIF_TERM
commit_lookup_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bsha;

  if (!enif_inspect_binary(env, argv[1], &bsha)) [[unlikely]]
    return raise_badarg_exception(env, argv[1]);

  std::string sha = bin_to_str(bsha);

  git_oid oid;
  if (git_oid_fromstr(&oid, sha.c_str()) < 0)
    return ATOM_NIL;

  std::vector<ERL_NIF_TERM> keys, vals;

  auto push = [&keys, &vals, env](ERL_NIF_TERM key, const char* val) {
    keys.push_back(key);
    vals.push_back(val ? make_binary(env, val) : ATOM_NIL);
  };

  auto pushi = [&keys, &vals, env](ERL_NIF_TERM key, int64_t val) {
    keys.push_back(key);
    vals.push_back(enif_make_int64(env, val));
  };

  auto pusht = [&keys, &vals](ERL_NIF_TERM key, ERL_NIF_TERM val) {
    keys.push_back(key);
    vals.push_back(val);
  };

  auto push_sign = [&, env](ERL_NIF_TERM key, git_signature const* val) {
    keys.push_back(key);
    vals.push_back(enif_make_tuple2(env, make_binary(env, val->name), make_binary(env, val->email)));
  };

  // Smart pointer that will automatically free the commit object
  SmartPtr<git_commit> commit(git_commit_free);

  if (git_commit_lookup(&commit, repo->get(), &oid) < 0)
    return raise_git_exception(env, "Failed to find git commit " + sha);

  ERL_NIF_TERM  head, list = argv[2];

  while (enif_get_list_cell(env, list, &head, &list)) {
    if      (enif_is_identical(head, ATOM_ENCODING))      push(ATOM_ENCODING,       git_commit_message_encoding(commit));
    else if (enif_is_identical(head, ATOM_MESSAGE))       push(ATOM_MESSAGE,        git_commit_message         (commit));
    else if (enif_is_identical(head, ATOM_SUMMARY))       push(ATOM_SUMMARY,        git_commit_summary         (commit));
    else if (enif_is_identical(head, ATOM_TIME))          pushi(ATOM_TIME,          git_commit_time            (commit));
    else if (enif_is_identical(head, ATOM_TIME_OFFSET))   pushi(ATOM_TIME_OFFSET,   git_commit_time_offset     (commit) * 60L);
    else if (enif_is_identical(head, ATOM_COMMITTER))     push_sign(ATOM_COMMITTER, git_commit_committer       (commit));
    else if (enif_is_identical(head, ATOM_AUTHOR))        push_sign(ATOM_AUTHOR,    git_commit_author          (commit));
    else if (enif_is_identical(head, ATOM_HEADER))        push(ATOM_HEADER,         git_commit_raw_header      (commit));
    else if (enif_is_identical(head, ATOM_TREE_ID))       pusht(ATOM_TREE_ID,       oid_to_bin(env, git_commit_tree_id(commit)));
    else [[unlikely]]
      return enif_make_badarg(env);
  }

  if (keys.empty())
    return enif_make_new_map(env);

  ERL_NIF_TERM map;
  if (!enif_make_map_from_arrays(env, &keys.front(), &vals.front(), keys.size(), &map)) [[unlikely]]
    return enif_raise_exception(env, ATOM_ENOMEM);

  return map;
}

static ERL_NIF_TERM clone_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary url, path;
  assert(argc >= 2 && argc <= 3);

  if (!enif_inspect_binary(env, argv[0], &url) ||
      !enif_inspect_binary(env, argv[1], &path)) [[unlikely]]
    return enif_make_badarg(env);

  std::string surl  = bin_to_str(url);
  std::string spath = bin_to_str(path);

  git_repository* p{};
  git_clone_options clone_opts = GIT_CLONE_OPTIONS_INIT;

  // Parse credentials from options if provided
  ERL_NIF_TERM credentials = 0;
  if (argc == 3) {
    credentials = extract_credentials_from_options(env, argv[2]);
    if (credentials && !parse_credentials_param(env, credentials)) [[unlikely]]
      return enif_make_badarg(env);
  }

  // Always install the credential callback - even with no credentials
  // supplied - so libgit2 falls back to ssh-agent/default credentials
  // (like the `git` CLI does) instead of refusing outright. This matters
  // for public https:// URLs too: local git config can rewrite them to an
  // SSH URL via `url.insteadOf`, which then requires *some* callback.
  set_clone_credentials_callback(env, &clone_opts, credentials);

  if (git_clone(&p, surl.c_str(), spath.c_str(), &clone_opts) < 0) [[unlikely]]
    return raise_git_exception(env, "Failed to clone git repo " + surl);

  #ifdef NIF_DEBUG
  fprintf(stderr, "=egit=> Cloned repo %p [%d]\r\n", p, __LINE__);
  #endif

  return to_monitored_resource(env, p);
}

static ERL_NIF_TERM init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  std::string path;
  auto        bare = false;

  // Parse options
  {
    auto opts = argv[1];

    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[0], &bin) || bin.size == 0) [[unlikely]]
      return enif_make_badarg(env);

    if (!enif_is_list(env, opts)) [[unlikely]]
      return enif_make_badarg(env);

    ERL_NIF_TERM opt;

    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if (enif_is_identical(opt, ATOM_BARE)) bare = true;
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }

    path = bin_to_str(bin);
  }

  git_repository* p{};

  if (git_repository_init(&p, path.c_str(), bare) != GIT_OK) [[unlikely]]
    return raise_git_exception(env, std::format("Failed to init git repo {}", path));

  #ifdef NIF_DEBUG
  fprintf(stderr, "=egit=> Init repo %p [%d]\r\n", p, __LINE__);
  #endif

  return to_monitored_resource(env, p);
}

static ERL_NIF_TERM open_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary path;
  assert(argc == 1);

  if (!enif_inspect_binary(env, argv[0], &path)) [[unlikely]]
    return enif_make_badarg(env);

  std::string spath = bin_to_str(path);

  git_repository* p{};

  if (git_repository_open(&p, spath.c_str()) < 0) [[unlikely]]
    return raise_git_exception(env, "Failed to open git repo " + spath);

  return to_monitored_resource(env, p);
}

static ERL_NIF_TERM fetch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  const char* fetch_or_pull = nullptr;

  if (enif_is_identical(argv[1], ATOM_FETCH))
    fetch_or_pull = "fetch";
  else if (enif_is_identical(argv[1], ATOM_PULL))
    fetch_or_pull = "pull";
  else
    return enif_make_badarg(env);

  std::string remote_name("origin");
  ERL_NIF_TERM credentials = 0;

  // Parse optional remote name and/or options, in the shapes the Erlang
  // wrappers may pass:
  //   argc == 2                          -> defaults only
  //   argc == 3, argv[2] is a map        -> options for the default remote
  //   argc == 3, argv[2] is a binary     -> remote name, no options
  //   argc == 4                         -> remote name (argv[2]) + options (argv[3])
  if (argc > 2) {
    if (enif_is_map(env, argv[2])) {
      if (argc > 3) [[unlikely]]
        return enif_make_badarg(env);
      credentials = extract_credentials_from_options(env, argv[2]);
    } else {
      ErlNifBinary bin;
      if (!enif_inspect_binary(env, argv[2], &bin)) [[unlikely]]
        return enif_make_badarg(env);
      remote_name = bin_to_str(bin);

      if (argc > 3) {
        if (!enif_is_map(env, argv[3])) [[unlikely]]
          return enif_make_badarg(env);
        credentials = extract_credentials_from_options(env, argv[3]);
      }
    }
  }

  if (credentials && !parse_credentials_param(env, credentials)) [[unlikely]]
    return enif_make_badarg(env);

  SmartPtr<git_remote> remote(git_remote_free);

  if (git_remote_lookup(&remote, repo->get(), remote_name.c_str()) < 0)
    return make_git_error(env, "Failed to lookup remote " + remote_name);

  // Always install the credential callback (see clone_nif for why) so
  // fetch/pull fall back to ssh-agent/default credentials instead of
  // refusing outright when no explicit credentials are supplied.
  git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;
  set_fetch_credentials_callback(env, &fetch_opts, credentials);

  if (git_remote_fetch(remote,
                       NULL,              // refspecs, NULL to use the configured ones
                       &fetch_opts,       // options
                       fetch_or_pull) < 0) // reflog message
    return make_git_error(env, "Failed to fetch from " + remote_name);

  return ATOM_OK;
}

static ERL_NIF_TERM cat_file_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  std::string filename(bin_to_str(bin));

  return lg2_cat_file(env, repo->get(), filename, argv[2]);
}

static ERL_NIF_TERM checkout_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  std::string rev = bin_to_str(bin);

  return lg2_checkout(env, repo->get(), rev, argv[2]);
}

static ERL_NIF_TERM add_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[1]) || !enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_add(env, repo->get(), argv[1], argv[2]);
}

static ERL_NIF_TERM commit_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_commit(env, repo->get(), bin_to_str(bin));
}

static ERL_NIF_TERM push_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc >= 3 && argc <= 4);

  // Parse options
  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  std::string sremote = "origin";

  if (!term_to_str(env, argv[1], sremote)) [[unlikely]]
    return enif_make_badarg(env);

  ERL_NIF_TERM ref, list = argv[2];

  if (!enif_is_list(env, list)) [[unlikely]]
    return raise_badarg_exception(env, list);

  std::vector<std::string> ref_specs;

  while (enif_get_list_cell(env, list, &ref, &list)) {
    std::string str;
    if (!term_to_str(env, ref, str)) [[unlikely]]
      return raise_badarg_exception(env, ref);
    ref_specs.push_back(str);
  }

  // Parse credentials from options if provided
  ERL_NIF_TERM credentials = 0;
  if (argc == 4) {
    credentials = extract_credentials_from_options(env, argv[3]);
  }

  if (credentials && !parse_credentials_param(env, credentials)) [[unlikely]]
    return enif_make_badarg(env);

  std::vector<const char*> cref_specs;
  for (auto& s : ref_specs)
    cref_specs.push_back(s.c_str());

  git_strarray refspecs = {
    .strings = cref_specs.empty() ? nullptr : const_cast<char**>(&cref_specs.front()),
    .count   = cref_specs.size()
  };

  SmartPtr<git_remote> remote(git_remote_free);
  if (git_remote_lookup(&remote, repo->get(), sremote.c_str()) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Unable to lookup remote");

  git_push_options push_opts;
  if (git_push_options_init(&push_opts, GIT_PUSH_OPTIONS_VERSION) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Error initializing push");

  // Always install the credential callback (see clone_nif for why) so
  // push falls back to ssh-agent/default credentials instead of refusing
  // outright when no explicit credentials are supplied.
  set_push_credentials_callback(env, &push_opts, credentials);

  return git_remote_push(remote, &refspecs, &push_opts) == GIT_OK
       ? ATOM_OK : make_git_error(env, "Error pushing to " + sremote);
}

static ERL_NIF_TERM rev_parse_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_rev_parse(env, repo->get(), bin_to_str(bin), argv[2]);
}

static ERL_NIF_TERM rev_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[1])) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_rev_list(env, repo->get(), argv[1], argv[2]);
}

static ERL_NIF_TERM config_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc >= 2);

  if (argc > 3) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_config(env, argv[0], argv[1], argc == 3 ? argv[2] : 0);
}

static ERL_NIF_TERM branch_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc >= 3 && argc <= 4);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ERL_NIF_TERM op = argv[1];

  return lg2_branch(env, repo->get(), op, argv[2], argc == 4 ? argv[3] : 0);
}

static ERL_NIF_TERM list_branches_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_branch_list(env, repo->get(), argv[1]);
}

static ERL_NIF_TERM list_index_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[1])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_index(env, repo->get(), argv[1]);
}

static ERL_NIF_TERM remote_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 4);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ERL_NIF_TERM op = argv[1];

  ErlNifBinary name;
  if (!enif_inspect_binary(env, argv[2], &name) || name.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!(enif_is_tuple(env, op) || enif_is_atom(env, op)) || !enif_is_list(env, argv[3])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_remote(env, repo->get(), bin_to_str(name), op, argv[3]);
}

static ERL_NIF_TERM list_remotes_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_remotes_list(env, repo->get());
}

static ERL_NIF_TERM tag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 4);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ERL_NIF_TERM op = argv[1];

  ErlNifBinary name;
  if (!enif_inspect_binary(env, argv[2], &name)) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_atom(env, op) || !enif_is_list(env, argv[3])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_tag(env, repo->get(), bin_to_str(name), op, argv[3]);
}

static ERL_NIF_TERM status_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[1])) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_status(env, repo->get(), argv[1]);
}

static ERL_NIF_TERM reset_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  auto type = git_reset_t(0);

  if      (enif_is_identical(ATOM_SOFT,  argv[1])) type = GIT_RESET_SOFT;
  else if (enif_is_identical(ATOM_HARD,  argv[1])) type = GIT_RESET_HARD;
  else if (enif_is_identical(ATOM_MIXED, argv[1])) type = GIT_RESET_MIXED;
  else [[unlikely]]
    return enif_make_badarg(env);

  std::string target;
  if (!term_to_str(env, argv[2], target)) [[unlikely]]
    return enif_make_badarg(env);

  SmartPtr<git_object> id(git_object_free);

  if (git_revparse_single(&id, repo->get(), target.c_str()) != GIT_OK) [[unlikely]]
    return make_git_error(env, std::format("Failed to lookup commit {}", target));

  return git_reset(repo->get(), id, type, nullptr) == GIT_OK
       ? ATOM_OK : make_git_error(env, "Cannot reset");
}

static ERL_NIF_TERM blame_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  std::string path = bin_to_str(bin);
  return lg2_blame(env, repo->get(), path, argv[2]);
}

static ERL_NIF_TERM describe_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[2])) [[unlikely]]
    return enif_make_badarg(env);

  std::string rev = bin_to_str(bin);
  return lg2_describe(env, repo->get(), rev, argv[2]);
}

static ERL_NIF_TERM cherry_pick_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string commit_oid = bin_to_str(bin);
  return lg2_cherry_pick(env, repo->get(), commit_oid);
}

static ERL_NIF_TERM reflog_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string refname = bin_to_str(bin);
  return lg2_reflog(env, repo->get(), refname);
}

static ERL_NIF_TERM remove_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string path = bin_to_str(bin);
  return lg2_remove(env, repo->get(), path);
}

static ERL_NIF_TERM move_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 3);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary old_bin, new_bin;
  if (!enif_inspect_binary(env, argv[1], &old_bin) || old_bin.size == 0 ||
      !enif_inspect_binary(env, argv[2], &new_bin) || new_bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string old_path = bin_to_str(old_bin);
  std::string new_path = bin_to_str(new_bin);
  return lg2_move(env, repo->get(), old_path, new_path);
}

static ERL_NIF_TERM diff_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 4);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary from_bin, to_bin;
  if (!enif_inspect_binary(env, argv[1], &from_bin) || from_bin.size == 0 ||
      !enif_inspect_binary(env, argv[2], &to_bin) || to_bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  if (!enif_is_list(env, argv[3])) [[unlikely]]
    return enif_make_badarg(env);

  std::string from_rev = bin_to_str(from_bin);
  std::string to_rev = bin_to_str(to_bin);
  return lg2_diff(env, repo->get(), from_rev, to_rev, argv[3]);
}

static ERL_NIF_TERM merge_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string branch_oid = bin_to_str(bin);
  return lg2_merge(env, repo->get(), branch_oid);
}

static ERL_NIF_TERM revert_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string commit_oid = bin_to_str(bin);
  return lg2_revert(env, repo->get(), commit_oid);
}

static ERL_NIF_TERM rebase_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string onto_ref = bin_to_str(bin);
  return lg2_rebase_init(env, repo->get(), onto_ref);
}

static ERL_NIF_TERM rebase_next_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_rebase_next(env, repo->get());
}

static ERL_NIF_TERM rebase_finish_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_rebase_finish(env, repo->get());
}

static ERL_NIF_TERM rebase_abort_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_rebase_abort(env, repo->get());
}

static ERL_NIF_TERM stash_save_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bin;
  if (!enif_inspect_binary(env, argv[1], &bin) || bin.size == 0) [[unlikely]]
    return enif_make_badarg(env);

  std::string message = bin_to_str(bin);
  return lg2_stash_save(env, repo->get(), message);
}

static ERL_NIF_TERM stash_list_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 1);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_stash_list(env, repo->get());
}

static ERL_NIF_TERM stash_apply_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifSInt64 index;
  if (!enif_get_int64(env, argv[1], &index) || index < 0) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_stash_apply(env, repo->get(), (size_t)index);
}

static ERL_NIF_TERM stash_pop_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifSInt64 index;
  if (!enif_get_int64(env, argv[1], &index) || index < 0) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_stash_pop(env, repo->get(), (size_t)index);
}

static ERL_NIF_TERM stash_drop_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  assert(argc == 2);

  GitRepoPtr* repo;
  if (!enif_get_resource(env, argv[0], GIT_REPO_RESOURCE, (void**)&repo)) [[unlikely]]
    return enif_make_badarg(env);

  ErlNifSInt64 index;
  if (!enif_get_int64(env, argv[1], &index) || index < 0) [[unlikely]]
    return enif_make_badarg(env);

  return lg2_stash_drop(env, repo->get(), (size_t)index);
}

static void resource_dtor(ErlNifEnv* env, void* arg)
{
  assert(arg);
  #ifdef NIF_DEBUG
  fprintf(stderr, "=egit=> Releasing resource %p [%d]\r\n", arg, __LINE__);
  #endif
  static_cast<GitRepoPtr*>(arg)->~GitRepoPtr();
}

static void resource_down(ErlNifEnv* env, void* obj, ErlNifPid*, ErlNifMonitor*)
{
  #ifdef NIF_DEBUG
  fprintf(stderr, "=egit=> Decremented resource ref %p [%d]\r\n", obj, __LINE__);
  #endif
  enif_release_resource(obj);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  init_atoms(env);

  auto flags                 = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  ErlNifResourceTypeInit rti = {.dtor = &resource_dtor, .down = &resource_down};
  GIT_REPO_RESOURCE          = enif_open_resource_type_x(env, "git_repo_resource",  &rti, flags, nullptr);

  git_libgit2_init();

  return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  //if (old_priv_data)
  //  enif_release_resource(old_priv_data);
  return 0;
}

static ErlNifFunc git_funcs[] =
{
  // Repository initialization - filesystem I/O to create .git structure
  {"init_nif",          2, init_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Repository cloning - network and filesystem I/O
  {"clone_nif",         2, clone_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"clone_nif",         3, clone_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Repository opening - filesystem I/O
  {"open_nif",          1, open_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Fetch operations - network I/O
  {"fetch_nif",         2, fetch_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"fetch_nif",         3, fetch_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"fetch_nif",         4, fetch_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Index operations - filesystem I/O
  {"add_nif",           3, add_nif,           ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Checkout operations - filesystem I/O to update working tree
  {"checkout_nif",      3, checkout_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Push operations - network I/O
  {"push_nif",          3, push_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"push_nif",          4, push_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Commit operations - filesystem I/O to write objects
  {"commit_nif",        2, commit_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Commit lookup - reads object database from filesystem
  {"commit_lookup_nif", 3, commit_lookup_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Cat file operations - filesystem I/O to read git objects
  {"cat_file_nif",      3, cat_file_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Revision parsing - filesystem I/O to resolve references
  {"rev_parse_nif",     3, rev_parse_nif,     ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Revision listing - filesystem I/O to enumerate commits
  {"rev_list_nif",      3, rev_list_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Configuration operations - filesystem I/O to read/write config files
  {"config_get_nif",    2, config_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"config_set_nif",    3, config_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Branch operations - filesystem I/O to manage branches
  {"branch_nif",        3, branch_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"branch_nif",        4, branch_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // List branches - filesystem I/O
  {"list_branches",     2, list_branches_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},

  // List index - filesystem I/O to read index file
  {"list_index",        2, list_index_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Remote operations - filesystem I/O to manage remote configurations
  {"remote_nif",        4, remote_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Tag operations - filesystem I/O
  {"tag_nif",           4, tag_nif,           ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Status operations - filesystem I/O to check working tree state
  {"status_nif",        2, status_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Reset operations - filesystem I/O to reset working tree and index
  {"reset_nif",         3, reset_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},

  // List remotes - filesystem I/O
  {"list_remotes",      1, list_remotes_nif,  ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Blame operations - filesystem I/O to compute blame history
  {"blame_nif",         3, blame_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Describe operations - filesystem I/O
  {"describe_nif",      3, describe_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Cherry-pick operations - filesystem I/O
  {"cherry_pick_nif",   2, cherry_pick_nif,   ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Reflog operations - filesystem I/O to read reflog
  {"reflog_nif",        2, reflog_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Remove from index - filesystem I/O
  {"remove_nif",        2, remove_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Move/rename in index - filesystem I/O
  {"move_nif",          3, move_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Diff operations - filesystem I/O to compute differences
  {"diff_nif",          4, diff_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Merge operations - filesystem I/O
  {"merge_nif",         2, merge_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Revert operations - filesystem I/O
  {"revert_nif",        2, revert_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Rebase operations - filesystem I/O
  {"rebase_init_nif",   2, rebase_init_nif,   ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"rebase_next_nif",   1, rebase_next_nif,   ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"rebase_finish_nif", 1, rebase_finish_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"rebase_abort_nif",  1, rebase_abort_nif,  ERL_NIF_DIRTY_JOB_IO_BOUND},

  // Stash operations - filesystem I/O
  {"stash_save_nif",    2, stash_save_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"stash_list_nif",    1, stash_list_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"stash_apply_nif",   2, stash_apply_nif,   ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"stash_pop_nif",     2, stash_pop_nif,     ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"stash_drop_nif",    2, stash_drop_nif,    ERL_NIF_DIRTY_JOB_IO_BOUND},
};

static void unload(ErlNifEnv* env, void* priv_data) {
  git_libgit2_shutdown();
}

ERL_NIF_INIT(git, git_funcs, load, NULL, upgrade, unload);
