//-----------------------------------------------------------------------------
// This code was derived from
// https://github.com/libgit2/libgit2/blob/main/examples/cat-file.c
//-----------------------------------------------------------------------------
// libgit2 "cat-file" - return data from the git ODB
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

static ERL_NIF_TERM print_signature(ErlNifEnv* env, const git_signature* sig)
{
  if (!sig)
    return ATOM_NIL;

  return enif_make_tuple4(env,
    make_binary(env, sig->name),
    make_binary(env, sig->email),
    enif_make_int64(env, sig->when.time),
    enif_make_int(env, sig->when.offset * 60));
}

// Printing out a blob is simple, get the contents and print
static ERL_NIF_TERM encode_blob(ErlNifEnv* env, const git_blob* blob)
{
  auto         data   = std::string_view((const char*)git_blob_rawcontent(blob), (size_t)git_blob_rawsize(blob));
  ERL_NIF_TERM keys[] = {ATOM_TYPE, ATOM_DATA};
  ERL_NIF_TERM vals[] = {ATOM_BLOB, make_binary(env, data)};
  ERL_NIF_TERM map;

  return enif_make_map_from_arrays(env, keys, vals, 2, &map) ? map : make_error(env, ATOM_ENOMEM);
}

/// Return each entry with its type, id and attributes
static ERL_NIF_TERM encode_tree(ErlNifEnv* env, const git_tree *tree, int abbrev)
{
  size_t i, max_i = (int)git_tree_entrycount(tree);
  const git_tree_entry *te;

  std::vector<ERL_NIF_TERM> v;
  v.reserve(max_i);

  for (i = 0; i < max_i; ++i) {
    te = git_tree_entry_byindex(tree, i);

    v.push_back(
      enif_make_tuple4(env,
        make_binary(env, git_tree_entry_name(te)),
        make_binary(env, git_object_type2string(git_tree_entry_type(te))),
        make_binary(env, oid_to_str(git_tree_entry_id(te), abbrev)),
        enif_make_int(env, git_tree_entry_filemode(te))));
  }

  ERL_NIF_TERM keys[] = {ATOM_TYPE, ATOM_COMMITS};
  ERL_NIF_TERM vals[] = {ATOM_TREE, enif_make_list_from_array(env, &v.front(), v.size())};
  ERL_NIF_TERM map;

  return enif_make_map_from_arrays(env, keys, vals, 2, &map) ? map : make_error(env, ATOM_ENOMEM);
}

// Commits and tags have a few interesting fields in their header.
static ERL_NIF_TERM encode_commit(ErlNifEnv* env, const git_commit* commit, int abbrev)
{
  auto max_i = (unsigned int)git_commit_parentcount(commit);
  std::vector<ERL_NIF_TERM> v;
  v.reserve(max_i);

  for (auto i = 0u; i < max_i; ++i)
    v.push_back(oid_to_bin_term(env, git_commit_parent_id(commit, i), abbrev));

  auto msg = git_commit_message(commit);

  ERL_NIF_TERM keys[] = {ATOM_TYPE, ATOM_OID, ATOM_PARENTS, ATOM_AUTHOR, ATOM_COMMITTER, ATOM_MESSAGE};
  ERL_NIF_TERM vals[] = {
    ATOM_COMMIT,
    oid_to_bin_term(env, git_commit_tree_id(commit), abbrev),
    enif_make_list_from_array(env, &v.front(), v.size()),
    print_signature(env, git_commit_author(commit)),
    print_signature(env, git_commit_committer(commit)),
    msg ? make_binary(env, msg) : ATOM_NIL
  };

  ERL_NIF_TERM map;

  return enif_make_map_from_arrays(env, keys, vals, msg ? 4 : 3, &map) ? map : make_error(env, ATOM_ENOMEM);
}

static ERL_NIF_TERM encode_tag(ErlNifEnv* env, const git_tag* tag, int abbrev)
{
  auto msg = git_tag_message(tag);

  ERL_NIF_TERM keys[] = {ATOM_TYPE, ATOM_OBJECT, ATOM_TYPE, ATOM_TAG, ATOM_TAGGER, ATOM_MESSAGE};

  ERL_NIF_TERM vals[] = {
    ATOM_TAG,
    oid_to_bin_term(env, git_tag_target_id(tag)),
    make_binary(env, git_object_type2string(git_tag_target_type(tag))),
    make_binary(env, git_tag_name(tag)),
    print_signature(env, git_tag_tagger(tag)),
    msg ? make_binary(env, msg) : ATOM_NIL
  };

  ERL_NIF_TERM map;

  return enif_make_map_from_arrays(env, keys, vals, msg ? 5 : 4, &map) ? map : make_error(env, ATOM_ENOMEM);
}

enum catfile_mode {
  SHOW_ALL,
  SHOW_TYPE,
  SHOW_SIZE,
};

// Entry point for this command
static ERL_NIF_TERM lg2_cat_file(ErlNifEnv* env, git_repository* repo, std::string const& rev, ERL_NIF_TERM opts)
{
  catfile_mode mode = SHOW_ALL;
  int          abbrev = GIT_OID_SHA1_HEXSIZE;

  // Parse options
  {
    int                 arity, n;
    ERL_NIF_TERM        opt;
    const ERL_NIF_TERM* tagvals;

    while (enif_get_list_cell(env, opts, &opt, &opts)) {
      if      (enif_is_identical(opt, ATOM_TYPE)) mode = SHOW_TYPE;
      else if (enif_is_identical(opt, ATOM_SIZE)) mode = SHOW_SIZE;
      else if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
        if (enif_is_identical(tagvals[0], ATOM_ABBREV) && enif_get_int(env, tagvals[1], &n) && n > 0 && n <= GIT_OID_SHA1_HEXSIZE)
          abbrev = n;
        else [[unlikely]]
          return raise_badarg_exception(env, opt);
      }
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
  }

  SmartPtr<git_object> obj(git_object_free);

  if (git_revparse_single(&obj, repo, rev.c_str()) != GIT_OK)
    return make_git_error(env, "Could not resolve " + rev);

  switch (mode) {
    case SHOW_TYPE:
      return enif_make_tuple2(env, ATOM_OK,
               enif_make_atom(env, git_object_type2string(git_object_type(obj))));
    case SHOW_SIZE: {
      SmartPtr<git_odb> odb(git_odb_free);
      if (git_repository_odb(&odb, repo) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Could not open ODB");

      SmartPtr<git_odb_object> odbobj(git_odb_object_free);
      if (git_odb_read(&odbobj, odb.get(), git_object_id(obj)) != GIT_OK) [[unlikely]]
        return make_git_error(env, "Could not find obj");

      return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, (long)git_odb_object_size(odbobj)));
    }
    default:
      switch (git_object_type(obj)) {
        case GIT_OBJECT_BLOB:   return encode_blob  (env, obj.template cast<const git_blob*>());
        case GIT_OBJECT_COMMIT: return encode_commit(env, obj.template cast<const git_commit*>(), abbrev);
        case GIT_OBJECT_TREE:   return encode_tree  (env, obj.template cast<const git_tree*>(), abbrev);
        case GIT_OBJECT_TAG:    return encode_tag   (env, obj.template cast<const git_tag*>(), abbrev);
        default:
          return make_git_error(env, std::format("Unknown object type {}", oid_to_str(obj)));
      }
      break;
  }

  return ATOM_OK;
}
