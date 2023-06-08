//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/checkout.c
//-----------------------------------------------------------------------------
// libgit2 "checkout" example - perform git checkouts
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

// Define the printf format specifier to use for size_t output
#if defined(_MSC_VER) || defined(__MINGW32__)
#  define PRIuZ "Iu"
#  define PRIxZ "Ix"
#  define PRIdZ "Id"
#else
#  define PRIuZ "zu"
#  define PRIxZ "zx"
#  define PRIdZ "zd"
#endif

struct checkout_options {
  checkout_options() : force(false), verbose(false), perf(false) {}
  bool force;      /// force the checkout to happen
  bool verbose;    /// show checkout progress
  bool perf;      /// show performance data
};

struct checkout_stats {
  int total_steps;
  int stat_calls;
  int mkdir_calls;
  int chmod_calls;
};

int resolve_refish(git_annotated_commit** commit, git_repository* repo, const char* refish)
{
  assert(commit != NULL);

  SmartPtr<git_reference> ref(git_reference_free);

  int err =  git_reference_dwim(&ref, repo, refish);
  if (err == GIT_OK) {
    git_annotated_commit_from_ref(commit, repo, ref);
    return GIT_OK;
  }

  SmartPtr<git_object> obj(git_object_free);

  err = git_revparse_single(&obj, repo, refish);
  if (err == GIT_OK)
    err = git_annotated_commit_lookup(commit, repo, git_object_id(obj));

  return err;
}

// This corresponds to `git switch --guess`: if a given ref does
// not exist, git will by default try to guess the reference by
// seeing whether any remote has a branch called <ref>. If there
// is a single remote only that has it, then it is assumed to be
// the desired reference and a local branch is created for it.
//
// The following is a simplified implementation. It will not try
// to check whether the ref is unique across all remotes.
static int guess_refish(git_annotated_commit** out, git_repository* repo, const char* ref)
{
  git_strarray remotes = { NULL, 0 };
  int err;

  if ((err = git_remote_list(&remotes, repo)) < 0)
    return err;

  auto cleanup = [&remotes]() { git_strarray_dispose(&remotes); };
  ScopeCleanup scope(cleanup);

  SmartPtr<git_reference> remote_ref(git_reference_free);

  for (size_t i = 0; i < remotes.count; i++) {
    auto refname = std::format("refs/remotes/{}/{}", remotes.strings[i], ref);
    if ((err = git_reference_lookup(&remote_ref, repo, refname.c_str())) < 0 && err != GIT_ENOTFOUND)
      break;
  }

  if (!remote_ref)
    return GIT_ENOTFOUND;

  return git_annotated_commit_from_ref(out, repo, remote_ref);
}

// Implementation of checkout logic
ERL_NIF_TERM lg2_checkout(ErlNifEnv* env, git_repository* repo, std::string const& rev, ERL_NIF_TERM opts)
{
  checkout_options o;
  int err = 0;

  // Parse options
  {
    ERL_NIF_TERM opt;

    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if      (enif_is_identical(opt, ATOM_VERBOSE)) o.verbose = true;
      else if (enif_is_identical(opt, ATOM_PERF))    o.perf    = true;
      else if (enif_is_identical(opt, ATOM_FORCE))   o.force   = true;
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
  }

  // Make sure we're not about to checkout while something else is going on
  auto state = git_repository_state(repo);
  if (state != GIT_REPOSITORY_STATE_NONE)
    return make_error(env, "Repository is in unexpected state " + std::to_string(state));

  SmartPtr<git_annotated_commit> target(git_annotated_commit_free);
  err = resolve_refish(&target, repo, rev.c_str()) != GIT_OK
     && guess_refish  (&target, repo, rev.c_str()) != GIT_OK;

  if (err)
    return make_git_error(env, "Failed to resolve " + rev);

  checkout_stats stats{};

  // This function is called to report progression, ie. it's called once with
  // a NULL path and the number of total steps, then for each subsequent path,
  // the current completed_step value.
  auto save_checkout_progress =
    [](const char* path, size_t completed_steps, size_t total_steps, void* payload)
    {
      auto stats = static_cast<checkout_stats*>(payload);

      if (payload)
        stats->total_steps = total_steps;
    };

  // This function is called when the checkout completes, and is used to report the
  // number of syscalls performed.
  auto save_perf_data =
    [](const git_checkout_perfdata* perfdata, void* payload)
    {
      auto stats = static_cast<checkout_stats*>(payload);

      if (stats) {
        stats->stat_calls  = perfdata->stat_calls;
        stats->mkdir_calls = perfdata->mkdir_calls;
        stats->chmod_calls = perfdata->chmod_calls;
      }
    };

  // This is the main "checkout <branch>" logic, responsible for performing
  // a branch-based checkout.
  git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
  checkout_opts.checkout_strategy    = o.force ? GIT_CHECKOUT_FORCE : GIT_CHECKOUT_SAFE;
  if (o.verbose) {
    checkout_opts.progress_cb        = save_checkout_progress;
    checkout_opts.progress_payload   = &stats;
    if (o.perf) {
      checkout_opts.perfdata_cb      = save_perf_data;
      checkout_opts.perfdata_payload = &stats;
    }
  }

  SmartPtr<git_commit> target_commit(git_commit_free);

  /// Grab the commit we're interested to move to
  if (git_commit_lookup(&target_commit, repo, git_annotated_commit_id(target.get())) < 0)
    return make_git_error(env, "Failed to lookup commit");

  // Perform the checkout so the workdir corresponds to what target_commit
  // contains.
  //
  // Note that it's okay to pass a git_commit here, because it will be
  // peeled to a tree.
  if (git_checkout_tree(repo, target_commit.template cast<const git_object*>(), &checkout_opts) < 0)
    return make_git_error(env, "Failed to checkout tree");

  // Now that the checkout has completed, we have to update HEAD.
  //
  // Depending on the "origin" of target (ie. it's an OID or a branch name),
  // we might need to detach HEAD.
  auto annotated_ref = git_annotated_commit_ref(target);

  if (!annotated_ref)
    err = git_repository_set_head_detached_from_annotated(repo, target) != GIT_OK;
  else {
    const char* target_head;

    SmartPtr<git_reference> ref(git_reference_free);
    SmartPtr<git_reference> branch(git_reference_free);

    if (git_reference_lookup(&ref, repo, annotated_ref) != GIT_OK)
      return make_git_error(env, "Failed to lookup annotated HEAD reference");

    if (!git_reference_is_remote(ref))
      target_head = annotated_ref;
    else if (git_branch_create_from_annotated(&branch, repo, rev.c_str(), target, 0) < 0)
      return make_git_error(env, "Failed to update HEAD reference from remote branch");
    else
      target_head = git_reference_name(branch);

    err = git_repository_set_head(repo, target_head) != GIT_OK;
  }

  if (err)
    return make_git_error(env, "Failed to update HEAD reference");

  if (!o.verbose)
    return ATOM_OK;

  ERL_NIF_TERM keys[] = {ATOM_TOTAL_STEPS, ATOM_STAT_CALLS, ATOM_MKDIR_CALLS, ATOM_CHMOD_CALLS};
  ERL_NIF_TERM vals[] = {
    enif_make_int(env, stats.total_steps),
    enif_make_int(env, stats.stat_calls),
    enif_make_int(env, stats.mkdir_calls),
    enif_make_int(env, stats.chmod_calls),
  };
  ERL_NIF_TERM map;
  return enif_make_map_from_arrays(env, keys, vals, 4, &map) ? map : enif_raise_exception(env, ATOM_ENOMEM);
}