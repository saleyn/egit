//-----------------------------------------------------------------------------
// libgit2 "branch" - add/rename/delete branches
//-----------------------------------------------------------------------------
#pragma once

#include <git2/common.h>

namespace {
  enum class BranchOp {
    CREATE,
    RENAME,
    DELETE,
  };

  enum NameKind {
    SHORT_NAME,
    FULL_NAME,
  };
}

ERL_NIF_TERM lg2_branch_list(ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM opts)
{
  int                     arity, n;
  ERL_NIF_TERM            opt;
  const ERL_NIF_TERM*     tagvals;
  auto                    limit = INT32_MAX;
  git_branch_t            list_flags = GIT_BRANCH_ALL;
  NameKind                kind = SHORT_NAME;

  // Parse options
  {
    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
        if (enif_is_identical(tagvals[0], ATOM_LIMIT) && enif_get_int(env, tagvals[1], &n))
          limit = n;
        else [[unlikely]]
          return raise_badarg_exception(env, opt);
      }
      else if (enif_is_identical(opt, ATOM_LOCAL))    list_flags = GIT_BRANCH_LOCAL;
      else if (enif_is_identical(opt, ATOM_REMOTE))   list_flags = GIT_BRANCH_REMOTE;
      else if (enif_is_identical(opt, ATOM_ALL))      list_flags = GIT_BRANCH_ALL;
      else if (enif_is_identical(opt, ATOM_FULLNAME)) kind       = FULL_NAME;
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
  }

  SmartPtr<git_branch_iterator> it(git_branch_iterator_free);
  if (git_branch_iterator_new(&it, repo, list_flags) != GIT_OK)
    return make_git_error(env, "Cannot create iterator");

  auto to_atom = [env](auto i) {
    switch (i) {
      case GIT_BRANCH_LOCAL:  return ATOM_LOCAL;
      case GIT_BRANCH_REMOTE: return ATOM_REMOTE;
      default:                return enif_make_int(env, i);
    }
  };

  std::vector<ERL_NIF_TERM> out;

  for (auto i=0; i < limit; ++i) {
    SmartPtr<git_reference> ref(git_reference_free);
    git_branch_t            out_type;

    switch (git_branch_next(&ref, &out_type, it)) {
      case GIT_ITEROVER:
        goto done;
      case GIT_OK:
        out.push_back(enif_make_tuple2(env, to_atom(out_type),
          make_binary(env, kind == SHORT_NAME ? git_reference_shorthand(ref) : git_reference_name(ref))));
        break;
      default:
        return make_git_error(env, "Failed to get next branch");
    }
  }
done:
  return enif_make_list_from_array(env, &out.front(), out.size());
}

ERL_NIF_TERM lg2_branch(
  ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM op, ERL_NIF_TERM name, ERL_NIF_TERM arg)
{
  BranchOp operation;

  if      (enif_is_identical(ATOM_CREATE, op) && arg && enif_is_list(env, arg))
    operation = BranchOp::CREATE;
  else if (enif_is_identical(ATOM_RENAME, op) && arg && enif_is_list(env, arg))
    operation = BranchOp::RENAME;
  else if (enif_is_identical(ATOM_DELETE, op) && !arg)
    operation = BranchOp::DELETE;
  else [[unlikely]]
    return enif_make_badarg(env);

  ErlNifBinary bname, bnew_name{};

  if (!enif_inspect_binary(env, name, &bname) || bname.size == 0) [[unlikely]]
    return raise_badarg_exception(env, name);

  std::string             sname(bin_to_str(bname));
  SmartPtr<git_reference> ref(git_reference_free);
  int                     arity;
  ERL_NIF_TERM            opt;
  const ERL_NIF_TERM*     tagvals;
  auto                    overwrite = false;

  switch (operation) {
    case BranchOp::CREATE: {
      std::string target("HEAD");
      // Parse options
      {
        while (enif_get_list_cell(env, arg, &opt, &arg)) {
          ErlNifBinary bin;
          if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
            if (enif_is_identical(tagvals[0], ATOM_TARGET) &&
                enif_inspect_binary(env, tagvals[1], &bin) && bin.size > 0 && bin.size <= GIT_OID_SHA1_HEXSIZE)
              target = bin_to_str(bin);
            else if (enif_is_identical(ATOM_OVERWRITE, opt))
              overwrite = true;
            else [[unlikely]]
              return raise_badarg_exception(env, opt);
          }
          else [[unlikely]]
            return raise_badarg_exception(env, opt);
        }
      }

      SmartPtr<git_object> id(git_object_free);
      SmartPtr<git_commit> target_commit(git_commit_free);

      if (git_revparse_single(&id, repo, target.c_str()) != GIT_OK)
        return make_git_error(env, std::format("Failed to lookup commit {}", target));

      // Grab the target commit we're interested in
      if (git_commit_lookup(&target_commit, repo, git_object_id(id)) < 0)
        return make_git_error(env, std::format("Failed to lookup commit {}", target));

      if (git_branch_create(&ref, repo, sname.c_str(), target_commit, overwrite) != GIT_OK) [[unlikely]]
        make_git_error(env, std::format("Failed to create branch {}", sname));

      break;
    }
    case BranchOp::RENAME: {
      std::string snew_name;

      while (enif_get_list_cell(env, arg, &opt, &arg)) {
        if (enif_is_identical(ATOM_OVERWRITE, opt)) {
          overwrite = true;
          continue;
        }
        if (!enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) [[unlikely]]
          return raise_badarg_exception(env, opt);
        if (enif_is_identical(tagvals[0], ATOM_NEW_NAME) &&
            enif_inspect_binary(env, tagvals[1], &bnew_name) && bnew_name.size > 0)
          snew_name = bin_to_str(bnew_name);
        else [[unlikely]]
          return raise_badarg_exception(env, opt);
      }

      if (git_branch_lookup(&ref, repo, sname.c_str(), GIT_BRANCH_ALL) != GIT_OK)
        return make_git_error(env, std::format("Failed to find branch {}", sname));

      SmartPtr<git_reference> out(git_reference_free);

      if (git_branch_move(&out, ref, snew_name.c_str(), overwrite) != GIT_OK) [[unlikely]]
        return make_git_error(env, std::format("Failed to rename branch {} to {}", sname, snew_name));

      break;
    }
    case BranchOp::DELETE: {
      if (git_branch_lookup(&ref, repo, sname.c_str(), GIT_BRANCH_ALL) != GIT_OK)
        return make_git_error(env, std::format("Failed to find branch {}", sname));

      if (git_branch_delete(ref) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Failed to delete branch");

      break;
    }
    default:
      assert(false);
  }

  return ATOM_OK;
}