//-----------------------------------------------------------------------------
// libgit2 "reflog" - show reference logs
//-----------------------------------------------------------------------------
#pragma once

#include <git2/reflog.h>

ERL_NIF_TERM lg2_reflog(ErlNifEnv* env, git_repository* repo, std::string const& refname)
{
  SmartPtr<git_reflog> reflog(git_reflog_free);

  if (git_reflog_read(&reflog, repo, refname.c_str()) != GIT_OK)
    return make_git_error(env, std::format("Failed to read reflog for {}", refname));

  size_t entry_count = git_reflog_entrycount(reflog);
  std::vector<ERL_NIF_TERM> entries;

  for (size_t i = 0; i < entry_count; ++i) {
    auto entry = git_reflog_entry_byindex(reflog, i);
    if (!entry) continue;

    auto sig = git_reflog_entry_committer(entry);
    const char* msg = git_reflog_entry_message(entry);

    auto entry_tuple = enif_make_tuple4(env,
      oid_to_bin_term(env, git_reflog_entry_id_new(entry)),
      make_binary(env, msg ? std::string_view(msg) : std::string_view("")),
      make_binary(env, sig ? std::string_view(sig->name) : std::string_view("unknown")),
      enif_make_int64(env, sig ? sig->when.time : 0)
    );
    entries.push_back(entry_tuple);
  }

  return make_list(env, entries);
}
