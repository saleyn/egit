/*
 * libgit2 "cat-file" example - shows how to print data from the ODB
 *
 * Written by the libgit2 contributors
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication along
 * with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */
#pragma once

#include <git2/common.h>

static ERL_NIF_TERM print_signature(ErlNifEnv* env, const git_signature* sig)
{
  /*
  char sign;
  int offset, hours, minutes;
  */

  if (!sig)
    return ATOM_NIL;

  /*
  offset = sig->when.offset;
  if (offset < 0) {
    sign = '-';
    offset = -offset;
  } else {
    sign = '+';
  }

  hours   = offset / 60;
  minutes = offset % 60;

  printf("%s %s <%s> %ld %c%02d%02d\r\n",
       header, sig->name, sig->email, (long)sig->when.time,
       sign, hours, minutes);
  */
  return enif_make_tuple4(env,
    make_binary(env, sig->name),
    make_binary(env, sig->email),
    enif_make_int64(env, sig->when.time),
    enif_make_int(env, sig->when.offset * 60));
}

// Printing out a blob is simple, get the contents and print
static ERL_NIF_TERM encode_blob(ErlNifEnv* env, const git_blob* blob)
{
  return enif_make_tuple2(env, ATOM_OK,
    enif_make_tuple2(env, ATOM_BLOB,
      make_binary(env,
        std::string_view((const char*)git_blob_rawcontent(blob), (size_t)git_blob_rawsize(blob)))));
}

/** Show each entry with its type, id and attributes */
static ERL_NIF_TERM encode_tree(ErlNifEnv* env, const git_tree *tree)
{
  size_t i, max_i = (int)git_tree_entrycount(tree);
  char oidstr[GIT_OID_SHA1_HEXSIZE + 1];
  const git_tree_entry *te;

  std::vector<ERL_NIF_TERM> v;
  v.reserve(max_i);

  for (i = 0; i < max_i; ++i) {
    te = git_tree_entry_byindex(tree, i);

    git_oid_tostr(oidstr, sizeof(oidstr), git_tree_entry_id(te));

    v.push_back(
      enif_make_tuple4(env,
        enif_make_int(env, git_tree_entry_filemode(te)),
        make_binary(env, git_object_type2string(git_tree_entry_type(te))),
        make_binary(env, oidstr),
        make_binary(env, git_tree_entry_name(te))));
  }

  return enif_make_tuple2(env, ATOM_OK,
    enif_make_tuple2(env, ATOM_TREE,
      enif_make_list_from_array(env, &v.front(), v.size())));
}

/**
 * Commits and tags have a few interesting fields in their header.
 */
static ERL_NIF_TERM encode_commit(ErlNifEnv* env, const git_commit* commit)
{
  char oidstr[GIT_OID_SHA1_HEXSIZE + 1];

  git_oid_tostr(oidstr, sizeof(oidstr), git_commit_tree_id(commit));
  printf("tree %s\r\n", oidstr);

  auto max_i = (unsigned int)git_commit_parentcount(commit);
  std::vector<ERL_NIF_TERM> v;
  v.reserve(max_i);

  for (auto i = 0u; i < max_i; ++i) {
    git_oid_tostr(oidstr, sizeof(oidstr), git_commit_parent_id(commit, i));
    v.push_back(make_binary(env, oidstr));
  }

  auto msg = git_commit_message(commit);

  ERL_NIF_TERM keys[] = {ATOM_PARENTS, ATOM_AUTHOR, ATOM_COMMITTER, ATOM_MESSAGE};
  ERL_NIF_TERM vals[] = {
    enif_make_list_from_array(env, &v.front(), v.size()),
    print_signature(env, git_commit_author(commit)),
    print_signature(env, git_commit_committer(commit)),
    msg ? make_binary(env, msg) : ATOM_NIL
  };

  ERL_NIF_TERM map;

  return enif_make_tuple2(env, ATOM_OK,
    enif_make_tuple2(env, ATOM_COMMIT,
      enif_make_map_from_arrays(env, keys, vals, msg ? 4 : 3, &map) ? map : ATOM_ENOMEM));
}

static ERL_NIF_TERM encode_tag(ErlNifEnv* env, const git_tag* tag)
{
  char oidstr[GIT_OID_SHA1_HEXSIZE + 1];

  git_oid_tostr(oidstr, sizeof(oidstr), git_tag_target_id(tag));;

  auto msg = git_tag_message(tag);

  ERL_NIF_TERM keys[] = {ATOM_OBJECT, ATOM_TYPE, ATOM_TAG, ATOM_TAGGER, ATOM_MESSAGE};

  ERL_NIF_TERM vals[] = {
    make_binary(env, oidstr),
    make_binary(env, git_object_type2string(git_tag_target_type(tag))),
    make_binary(env, git_tag_name(tag)),
    print_signature(env, git_tag_tagger(tag)),
    msg ? make_binary(env, msg) : ATOM_NIL
  };

  ERL_NIF_TERM map;

  return enif_make_tuple2(env, ATOM_OK,
    enif_make_tuple2(env, ATOM_TAG,
      enif_make_map_from_arrays(env, keys, vals, msg ? 5 : 4, &map) ? map : ATOM_ENOMEM));
}

enum catfile_mode {
  SHOW_ALL,
  SHOW_TYPE,
  SHOW_SIZE,
};

/* Forward declarations for option-parsing helper */
struct catfile_options {
  catfile_options() : action(SHOW_ALL), dir(".") {}

  catfile_mode action;
  std::string  dir;
};

// Parse the command-line options taken from git
static ERL_NIF_TERM
parse_opts(ErlNifEnv* env, ERL_NIF_TERM opt, catfile_options& o)
{
  const ERL_NIF_TERM* tagval;
  int                 arity;

  o.action = SHOW_ALL;

  if      (enif_is_identical(opt, ATOM_TYPE)) o.action  = SHOW_TYPE;
  else if (enif_is_identical(opt, ATOM_SIZE)) o.action  = SHOW_SIZE;
  else if (enif_is_identical(opt, ATOM_ALL))  o.action  = SHOW_ALL;
  else if (enif_get_tuple(env, opt, &arity, &tagval) && arity == 2) {
    ErlNifBinary bin;
    if (enif_is_identical(tagval[0], ATOM_DIR) && enif_inspect_binary(env, tagval[1], &bin))
      o.dir = std::string((char*)bin.data, bin.size);
    else if (enif_is_identical(tagval[0], ATOM_REV) && enif_inspect_binary(env, tagval[1], &bin))
      o.dir = std::string((char*)bin.data, bin.size);
    else [[unlikely]]
      return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, opt));
  }
  else [[unlikely]]
    return enif_raise_exception(env, enif_make_tuple2(env, ATOM_BADARG, opt));

  return 0;
}

/** Entry point for this command */
static ERL_NIF_TERM cat_file(ErlNifEnv* env, git_repository* repo, std::string const& rev, ERL_NIF_TERM list)
{
  catfile_options o;

  auto res = parse_opts(env, list, o);

  if (res != 0) [[unlikely]]
    return res;

  git_object* obj = NULL;

  if (git_revparse_single(&obj, repo, rev.c_str()) < 0)
    return make_error(env, "Could not resolve " + rev);

  SMART_PTR(obj, git_object_free);
 
  switch (o.action) {
    case SHOW_TYPE:
      return enif_make_tuple2(env, ATOM_OK,
               enif_make_atom(env, git_object_type2string(git_object_type(obj))));
    case SHOW_SIZE: {
      git_odb*        odb;
      git_odb_object* odbobj;

      if (git_repository_odb(&odb, repo) < 0) [[unlikely]]
        return make_error(env, "Could not open ODB");

      SMART_PTR(odb, git_odb_free);

      if (git_odb_read(&odbobj, odb, git_object_id(obj))) [[unlikely]]
        return make_error(env, "Could not find obj");

      SMART_PTR(odbobj, git_odb_object_free);

      return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, (long)git_odb_object_size(odbobj)));
    }
    case SHOW_ALL:
      switch (git_object_type(obj)) {
        case GIT_OBJECT_BLOB:   return encode_blob  (env, (const git_blob*)  obj);
        case GIT_OBJECT_COMMIT: return encode_commit(env, (const git_commit*)obj);
        case GIT_OBJECT_TREE:   return encode_tree  (env, (const git_tree*)  obj);
        case GIT_OBJECT_TAG:    return encode_tag   (env, (const git_tag*)   obj);
        default: {
          char oidstr[GIT_OID_SHA1_HEXSIZE + 1];
          git_oid_tostr(oidstr, sizeof(oidstr), git_object_id(obj));
          char buf[256];
          snprintf(buf, sizeof(buf), "unknown object type %s", oidstr);
          return make_error(env, buf);
        }
      }
      break;
    default:
      break;
  }

  return ATOM_OK;
}
