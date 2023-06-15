//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/tag.c
//-----------------------------------------------------------------------------
// libgit2 "tag" - create/delete/list tags
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
  enum class TagOp {
    CREATE,
    DELETE,
    LIST,
  };
}

// Return the end of individual message lines
static const char* end_of_lines(const char *message, int num_lines)
{
  auto msg = message;
  auto num = num_lines - 1;

  if (!msg) [[unlikely]]
    return msg;

  while (*msg && *msg != '\n');         // first line - headline
  while (*msg && *msg == '\n') msg++;   // skip over new lines

  if (num == 0) return msg;             // just headline?

  // include individual commit/tag lines
  while (*msg && num-- >= 2) {
    while (*msg && *msg != '\n') msg++;

    // handle consecutive new lines
    if (*msg && *msg == '\n' && msg[1] == '\n')
      num--;

    while(*msg && *msg == '\n') msg++;
  }

  return msg;
}

// Implementation of "add" logic
ERL_NIF_TERM lg2_tag(ErlNifEnv* env, git_repository* repo, std::string const& name, ERL_NIF_TERM op, ERL_NIF_TERM opts)
{
  assert(repo);

  TagOp       action;
  std::string pattern("*"), comment, target;
  size_t      num_lines = 0;
  auto        force = false;

  if      (enif_is_identical(op, ATOM_LIST))   action = TagOp::LIST;
  else if (enif_is_identical(op, ATOM_CREATE)) action = TagOp::CREATE;
  else if (enif_is_identical(op, ATOM_DELETE)) action = TagOp::DELETE;
  else [[unlikely]]
    return raise_badarg_exception(env, op);

  // Parse args
  for (ERL_NIF_TERM opt; enif_get_list_cell(env, opts, &opt, &opts);) {
    int arity;
    const ERL_NIF_TERM* tagvals;

    if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
      if (!parse_if(env, ATOM_MESSAGE, tagvals, comment) &&
          !parse_if(env, ATOM_PATTERN, tagvals, pattern) &&
          !parse_if(env, ATOM_TARGET,  tagvals, target)  &&
          !parse_if(env, ATOM_LINES,   tagvals, num_lines)) [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
    else if (enif_is_identical(ATOM_FORCE, opt))
      force = true;
    else [[unlikely]]
      return raise_badarg_exception(env, opt);
  }

  ERL_NIF_TERM out;

  switch (action) {
    case TagOp::LIST: {
      out = enif_make_list(env, 0);
      GitStrArray tag_names;
      if (git_tag_list_match(&tag_names, pattern.c_str(), repo) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Unable to get list of tags");

      std::vector<ERL_NIF_TERM> res;
      res.reserve(tag_names.size());

      for (auto i=0u; i < tag_names.size(); ++i) {
        auto tag_name = tag_names[i];

        SmartPtr<git_object> obj(git_object_free);

        if (git_revparse_single(&obj, repo, tag_name) != GIT_OK) [[unlikely]]
          return make_git_error(env, "Failed to lookup rev");

        switch (git_object_type(obj)) {
          case GIT_OBJECT_TAG: {
            auto tag = obj.template cast<git_tag*>();
            auto msg = git_tag_message(tag);
            auto end = end_of_lines(msg, num_lines);
            auto ttt = make_binary(env, git_tag_name(tag));
            res.push_back(
              (end-msg) > 0
                ? enif_make_tuple2(env, ttt,
                    make_binary(env, std::string_view(msg, end-msg)))
                : ttt);
            break;
          }
          case GIT_OBJECT_COMMIT: {
            auto cmt = obj.template cast<git_commit*>();
            auto msg = git_commit_message(cmt);
            auto end = end_of_lines(msg, num_lines);
            auto ttt = make_binary(env, tag_name);
            res.push_back(
              (end-msg) > 0
                ? enif_make_tuple2(env, ttt,
                    make_binary(env, std::string_view(msg, end-msg)))
                : ttt);
            break;
          }
          default:
            res.push_back(make_binary(env, tag_name));
            break;
        }

        out = enif_make_list_from_array(env, &res.front(), res.size());
      }
      break;
    }
    case TagOp::CREATE: {
      if (name.empty()) [[unlikely]]
        return raise_badarg_exception(env, ATOM_NAME);

      if (target.empty()) target = "HEAD";

      SmartPtr<git_object> target_obj(git_object_free);
      if (git_revparse_single(&target_obj, repo, target.c_str()) != GIT_OK) [[unlikely]]
        return make_git_error(env, std::format("Unable to resolve spec {}", target));

      git_oid oid;
      ERL_NIF_TERM res;

      if (comment.empty()) // Create lightweight tag
        res = git_tag_create_lightweight(&oid, repo, name.c_str(), target_obj, force);
      else {
        SmartPtr<git_signature> tagger(git_signature_free);
        if (git_signature_default(&tagger, repo) != GIT_OK) [[unlikely]]
          return make_git_error(env, "Unable to create signature");

        res = git_tag_create(&oid, repo, name.c_str(), target_obj, tagger, comment.c_str(), force);
      }

      if (res != GIT_OK) [[unlikely]]
        return make_git_error(env, "Unable to create tag");

      out = ATOM_OK;
      break;
    }
    case TagOp::DELETE: {
      if (name.empty()) [[unlikely]]
        return raise_badarg_exception(env, ATOM_NAME);

      if (git_tag_delete(repo, name.c_str()) != GIT_OK)
        return make_git_error(env, std::format("Unable to delete tag {}", name));

      out = ATOM_OK;
      break;
    }
    default:
      return enif_make_badarg(env);
  }

  return out;
}