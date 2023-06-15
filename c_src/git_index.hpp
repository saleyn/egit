//-----------------------------------------------------------------------------
// libgit2 "index" - list index
//-----------------------------------------------------------------------------

// Implementation of "add" logic
ERL_NIF_TERM lg2_index(ErlNifEnv* env, git_repository* repo, ERL_NIF_TERM opts)
{
  assert(repo);

  auto abbrev        = GIT_OID_SHA1_HEXSIZE;
  auto all           = false;
  auto show_path     = false;
  auto show_stage    = false;
  auto show_conflict = false;
  auto show_oid      = false;
  auto show_mode     = false;
  auto show_size     = false;
  auto show_ctime    = false;
  auto show_mtime    = false;
  auto count         = 0;

  for (ERL_NIF_TERM opt; enif_get_list_cell(env, opts, &opt, &opts);) {
    int arity, n;
    const ERL_NIF_TERM* tagvals;
    if (enif_get_tuple(env, opt, &arity, &tagvals) && arity == 2) {
      if (enif_is_identical(tagvals[0], ATOM_ABBREV) && enif_get_int(env, tagvals[1], &n) && n > 0 && n <= GIT_OID_SHA1_HEXSIZE)
        abbrev = n;
      else if (enif_is_identical(tagvals[0], ATOM_FIELDS) && enif_is_identical(tagvals[1], ATOM_ALL))
        all = true;
      else if (enif_is_identical(tagvals[0], ATOM_FIELDS) && enif_is_list(env, tagvals[1]))
        for (ERL_NIF_TERM cell, list = tagvals[1]; enif_get_list_cell(env, list, &cell, &list);) {
          if      (enif_is_identical(cell, ATOM_PATH))     { all = false; show_path     = true; count++; }
          else if (enif_is_identical(cell, ATOM_STAGE))    { all = false; show_stage    = true; count++; }
          else if (enif_is_identical(cell, ATOM_CONFLICT)) { all = false; show_conflict = true; count++; }
          else if (enif_is_identical(cell, ATOM_OID))      { all = false; show_oid      = true; count++; }
          else if (enif_is_identical(cell, ATOM_MODE))     { all = false; show_mode     = true; count++; }
          else if (enif_is_identical(cell, ATOM_SIZE))     { all = false; show_size     = true; count++; }
          else if (enif_is_identical(cell, ATOM_CTIME))    { all = false; show_ctime    = true; count++; }
          else if (enif_is_identical(cell, ATOM_MTIME))    { all = false; show_mtime    = true; count++; }
          else [[unlikely]]
            return raise_badarg_exception(env, opt);
        }
      else [[unlikely]]
        return raise_badarg_exception(env, opt);
    }
    else [[unlikely]]
      return raise_badarg_exception(env, opts);
  }

  if (!count)
    show_path = true;

  SmartPtr<git_index> index(git_index_free);

  if (git_repository_index(&index, repo) != GIT_OK) [[unlikely]]
    return make_git_error(env, "Could not open repository index");

  git_index_read(index, 0);

  auto ecount = git_index_entrycount(index);
  if (!ecount)
    return enif_make_list(env, 0);

  std::vector<ERL_NIF_TERM> keys;
  keys.reserve(count);
  if (all || show_path)     keys.push_back(ATOM_PATH);
  if (all || show_stage)    keys.push_back(ATOM_STAGE);
  if (all || show_conflict) keys.push_back(ATOM_CONFLICT);
  if (all || show_oid)      keys.push_back(ATOM_OID);
  if (all || show_mode)     keys.push_back(ATOM_MODE);
  if (all || show_size)     keys.push_back(ATOM_SIZE);
  if (all || show_ctime)    keys.push_back(ATOM_CTIME);
  if (all || show_mtime)    keys.push_back(ATOM_MTIME);

  auto stage = [](auto s) {
    switch (git_index_stage_t(s)) {
      case GIT_INDEX_STAGE_NORMAL:   return ATOM_NORMAL;
      case GIT_INDEX_STAGE_ANCESTOR: return ATOM_ANCESTOR;
      case GIT_INDEX_STAGE_OURS:     return ATOM_OURS;
      case GIT_INDEX_STAGE_THEIRS:   return ATOM_THEIRS;
      default:                       return ATOM_ANY;
    }
  };

  std::vector<ERL_NIF_TERM> res;
  res.reserve(ecount);

  for (auto i = 0u; i < ecount; ++i) {
    auto e = git_index_get_byindex(index, i);

    std::vector<ERL_NIF_TERM> vals;
    vals.reserve(count);
    if (all || show_path)     vals.push_back(make_binary(env, e->path));
    if (all || show_stage)    vals.push_back(stage(git_index_entry_stage(e)));
    if (all || show_conflict) vals.push_back(git_index_entry_is_conflict(e) ? ATOM_TRUE : ATOM_FALSE);
    if (all || show_oid)      vals.push_back(oid_to_bin_term(env, &e->id, abbrev));
    if (all || show_mode)     vals.push_back(enif_make_int(env, e->mode));
    if (all || show_size)     vals.push_back(enif_make_int64(env, e->file_size));
    if (all || show_ctime)    vals.push_back(enif_make_int64(env, e->ctime.seconds));
    if (all || show_mtime)    vals.push_back(enif_make_int64(env, e->mtime.seconds));

    assert(keys.size() == vals.size());

    ERL_NIF_TERM map;
    if (!enif_make_map_from_arrays(env, &keys.front(), &vals.front(), vals.size(), &map)) [[unlikely]]
      make_error(env, ATOM_ENOMEM);

    res.push_back(map);
  }

  return enif_make_list_from_array(env, &res.front(), res.size());
}