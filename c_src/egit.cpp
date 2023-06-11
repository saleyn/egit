#include <stdio.h>
#include <cstring>
#include <cassert>
#include <memory>
#include <tuple>
#include <vector>
#include <atomic>

#ifndef USE_FMT_LIB
#include <format>
#else
#include <fmt/format.h>
namespace std { using namespace fmt; }
#endif

#ifndef GIT_OID_SHA1_HEXSIZE
#define GIT_OID_SHA1_HEXSIZE GIT_OID_HEXSZ
#endif

#if !defined(GIT_REVSPEC_MERGE_BASE) && defined(GIT_REVPARSE_MERGE_BASE)
#define GIT_REVSPEC_MERGE_BASE GIT_REVPARSE_MERGE_BASE
#endif

#include <git2.h>
#include "egit_utils.hpp"
#include "egit_add.hpp"
#include "egit_cat_file.hpp"
#include "egit_checkout.hpp"
#include "egit_commit.hpp"
#include "egit_rev_parse.hpp"
#include "egit_rev_list.hpp"
#include "egit_config.hpp"
#include "egit_branch.hpp"

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

  ERL_NIF_TERM map;
  if (!enif_make_map_from_arrays(env, &keys.front(), &vals.front(), keys.size(), &map)) [[unlikely]]
    return enif_raise_exception(env, ATOM_ENOMEM);

  return map;
}

static ERL_NIF_TERM clone_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary url, path;
  assert(argc == 2);

  if (!enif_inspect_binary(env, argv[0], &url) ||
      !enif_inspect_binary(env, argv[1], &path)) [[unlikely]]
    return enif_make_badarg(env);

  std::string surl  = bin_to_str(url);
  std::string spath = bin_to_str(path);

  git_repository* p{};

  if (git_clone(&p, surl.c_str(), spath.c_str(), nullptr) < 0) [[unlikely]]
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

  if (argc > 2) {
    ErlNifBinary bin;
    if (!enif_inspect_binary(env, argv[2], &bin)) [[unlikely]]
      return enif_make_badarg(env);

    remote_name = bin_to_str(bin);
  }

  git_remote* remote;

  if (git_remote_lookup(&remote, repo->get(), remote_name.c_str()) < 0)
    return make_git_error(env, "Failed to lookup remote " + remote_name);
  if (git_remote_fetch(remote,
                       NULL,               // refspecs, NULL to use the configured ones
                       NULL,               // options, empty for defaults
                       fetch_or_pull) < 0) // reflog mesage, "fetch" (or NULL) or "pull"
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

static ErlNifFunc egit_funcs[] =
{
  {"init_nif",          2, init_nif},
  {"clone_nif",         2, clone_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"open_nif",          1, open_nif,          ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"fetch_nif",         2, fetch_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"fetch_nif",         3, fetch_nif,         ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"add_nif",           3, add_nif,           ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"checkout_nif",      3, checkout_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"commit_nif",        2, commit_nif,        ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"cat_file_nif",      3, cat_file_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"rev_parse_nif",     3, rev_parse_nif,     ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"rev_list_nif",      3, rev_list_nif,      ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"config_get_nif",    2, config_nif},
  {"config_set_nif",    3, config_nif},
  {"branch_nif",        3, branch_nif},
  {"branch_nif",        4, branch_nif},
  {"list_branches",     2, list_branches_nif, ERL_NIF_DIRTY_JOB_IO_BOUND},
  {"commit_lookup_nif", 3, commit_lookup_nif, 0},
};

ERL_NIF_INIT(git, egit_funcs, load, NULL, upgrade, NULL);
