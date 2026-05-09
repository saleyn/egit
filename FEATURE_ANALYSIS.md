# Git Function Analysis: Missing Features for egit NIF Library

## Current Implementation Overview

The egit library currently exposes the following Git functionality through NIFs:

### Repository Management
- `init/1,2` - Initialize repositories (with bare option)
- `clone/2` - Clone remote repositories
- `open/1` - Open local repositories

### Remote Operations
- `fetch/1,2` - Fetch from remotes
- `pull/1,2` - Pull from remotes
- `push/1,2,3` - Push to remotes with optional ref specs
- `remote_add/3` - Add remotes
- `remote_rename/3` - Rename remotes
- `remote_delete/2` - Delete remotes
- `remote_set_url/3,4` - Change remote URLs
- `list_remotes/1` - List remotes

### Branch Management
- `branch_create/2,3` - Create branches with optional target
- `branch_rename/3,4` - Rename branches
- `branch_delete/2` - Delete branches
- `list_branches/1,2` - List branches (local/remote/all with filters)

### Tag Management
- `tag_create/2,3,4` - Create annotated/lightweight tags
- `tag_delete/2` - Delete tags
- `list_tags/1,2` - List tags with pattern/line filtering

### File Operations
- `add/2,3` - Add files to index with options
- `add_all/1` - Add all changes
- `checkout/2,3` - Checkout revisions/branches
- `status/1,2` - Get repository status

### Commit Operations
- `commit/2` - Commit staged changes
- `commit_lookup/3` - Look up commit details
- `rev_parse/2,3` - Parse revision specifications
- `rev_list/3` - List commits with sorting/filtering

### Configuration
- `config_get/2` - Get config values (system/global/local/app)
- `config_set/3` - Set config values

### Inspection
- `cat_file/2,3` - Inspect blobs, trees, commits, tags
- `list_index/1,2` - List index entries with field filtering

### Repository Maintenance
- `reset/2,3` - Reset to commits (soft/mixed/hard)

---

## Recommended Missing Functions

### High Priority (Commonly Used)

#### 1. **Blame** (`git blame`)
**Purpose**: Show who made each line change and when
**libgit2 API**: `git_blame_file()`, `git_blame_get_hunk_byline()`
**Use Cases**:
- Finding who introduced a bug
- Code review and history investigation
- Compliance tracking
**Return**: Map of line numbers to {author, email, commit_oid, time}
**Complexity**: Medium

```erlang
git:blame(Repo, Path) -> {ok, [{Line, Author, Email, CommitOID, Time}]} | {error, Reason}
git:blame(Repo, Path, [{oldest_commit, Ref}]) -> ...
```

#### 2. **Diff** (`git diff`)
**Purpose**: Compare revisions or working directory with index
**libgit2 API**: `git_diff_tree_to_index()`, `git_diff_tree_to_tree()`, `git_diff_index_to_workdir()`
**Use Cases**:
- Show staged vs. unstaged changes
- Compare branches before merging
- Pre-commit validation
**Return**: List of {file, {old_mode, new_mode}, additions, deletions, hunks}
**Complexity**: High (complex diff format)

```erlang
git:diff(Repo, FromRev, ToRev) -> {ok, DiffMap} | {error, Reason}
git:diff(Repo, FromRev, ToRev, [{context_lines, 3}]) -> ...
```

#### 3. **Merge** (`git merge`)
**Purpose**: Merge branches together
**libgit2 API**: `git_merge()`, `git_merge_analysis()`, `git_index_write_tree()`
**Use Cases**:
- Automating CI/CD merge operations
- Feature branch integration
- Conflict detection
**Return**: `ok | {conflict, ConflictList} | {error, Reason}`
**Complexity**: High (conflict resolution required)

```erlang
git:merge(Repo, Branch) -> ok | {conflict, Conflicts} | {error, Reason}
git:merge(Repo, OID, [{fastforward_only, true}]) -> ...
```

#### 4. **Rebase** (`git rebase`)
**Purpose**: Reapply commits on top of another branch
**libgit2 API**: `git_rebase_init()`, `git_rebase_next()`, `git_rebase_finish()`
**Use Cases**:
- Automated branch history cleanup
- Feature branch integration
- CI/CD automation
**Return**: `ok | {conflict, ConflictList} | {error, Reason}`
**Complexity**: High

```erlang
git:rebase(Repo, Onto) -> ok | {conflict, Conflicts} | {error, Reason}
git:rebase_continue(Repo) -> ok | {conflict, Conflicts} | {error, Reason}
git:rebase_abort(Repo) -> ok | {error, Reason}
```

#### 5. **Cherry-pick** (`git cherry-pick`)
**Purpose**: Apply a single commit's changes to current branch
**libgit2 API**: `git_cherrypick()`
**Use Cases**:
- Selective commit application
- Hotfix distribution across branches
- Commit history curation
**Return**: `ok | {conflict, ConflictList} | {error, Reason}`
**Complexity**: Medium

```erlang
git:cherry_pick(Repo, CommitOID) -> ok | {conflict, Conflicts} | {error, Reason}
```

#### 6. **Stash** (`git stash`)
**Purpose**: Temporarily save uncommitted changes
**libgit2 API**: `git_stash_save()`, `git_stash_apply()`, `git_stash_pop()`, `git_stash_list()`
**Use Cases**:
- Temporary work saving
- Branch switching with dirty state
- Backup of experimental changes
**Return**: `ok | {error, Reason}` / `[StashList]`
**Complexity**: Medium

```erlang
git:stash_save(Repo, "WIP: feature work") -> ok | {error, Reason}
git:stash_list(Repo) -> [{OID, Message}]
git:stash_apply(Repo, StashIndex) -> ok | {conflict, Conflicts}
git:stash_pop(Repo, StashIndex) -> ok | {conflict, Conflicts}
```

#### 7. **Revert** (`git revert`)
**Purpose**: Create new commits that undo changes from specified commits
**libgit2 API**: `git_revert()`
**Use Cases**:
- Safe commit undoing (preserves history)
- Fixing mistakes while maintaining git history
- Publishing previous errors
**Return**: `ok | {conflict, ConflictList} | {error, Reason}`
**Complexity**: Medium

```erlang
git:revert(Repo, CommitOID) -> ok | {conflict, Conflicts} | {error, Reason}
```

### Medium Priority (Often Used)

#### 8. **Reflog** (`git reflog`)
**Purpose**: Show reference logs (history of branch movements)
**libgit2 API**: `git_reflog_read()`, `git_reflog_entry_*()`
**Use Cases**:
- Finding lost commits
- Undo operations after force-push
- Debugging branch history
**Return**: `[{OID, Action, Message}]`
**Complexity**: Low

```erlang
git:reflog(Repo, Ref) -> [{OID, Action, Message}]
```

#### 9. **Describe** (`git describe`)
**Purpose**: Find most recent tag and describe current position
**libgit2 API**: `git_describe_commit()`, `git_describe_format()`
**Use Cases**:
- Version number generation
- Build version tagging
- Release identification
**Return**: `binary()` (e.g., "v1.0.0-5-gabcd123")
**Complexity**: Low

```erlang
git:describe(Repo, CommitOID) -> {ok, Description} | {error, Reason}
git:describe(Repo, CommitOID, [{pattern, "v*"}]) -> ...
```

#### 10. **Remove** (`git rm`)
**Purpose**: Remove files from index and working directory
**libgit2 API**: `git_index_remove()`, direct file removal
**Use Cases**:
- File cleanup before commit
- Tracking file deletion
- Moving files
**Return**: `ok | {error, Reason}`
**Complexity**: Low

```erlang
git:remove(Repo, FilePath) -> ok | {error, Reason}
git:remove(Repo, [FilePath1, FilePath2]) -> ok | {error, Reason}
```

#### 11. **Move** (`git mv`)
**Purpose**: Move/rename tracked files
**libgit2 API**: `git_index_remove()` + `git_index_add()`
**Use Cases**:
- File reorganization
- Refactoring-related moves
- Directory structure changes
**Return**: `ok | {error, Reason}`
**Complexity**: Low

```erlang
git:move(Repo, OldPath, NewPath) -> ok | {error, Reason}
```

#### 12. **Clean** (`git clean`)
**Purpose**: Remove untracked files from working directory
**libgit2 API**: Direct file system operations with git_status
**Use Cases**:
- Build cleanup
- Repository preparation
- Temporary file removal
**Return**: `ok | {error, Reason}`
**Complexity**: Medium (safety considerations)

```erlang
git:clean(Repo) -> ok | {error, Reason}
git:clean(Repo, [{force, true}, {directories, true}]) -> ok | {error, Reason}
```

#### 13. **Verify** (`git verify-commit` / `git verify-tag`)
**Purpose**: Check GPG signatures on commits and tags
**libgit2 API**: `git_commit_extract_signature()` (limited support in older versions)
**Use Cases**:
- Signed release verification
- Commit authenticity checking
- Security validation
**Return**: `{ok, Signature} | {error, unsigned}`
**Complexity**: Medium (GPG integration dependent)

```erlang
git:verify_commit(Repo, CommitOID) -> {ok, Signature} | {error, Reason}
git:verify_tag(Repo, TagName) -> {ok, Signature} | {error, Reason}
```

### Lower Priority (Specialized)

#### 14. **Submodules** (`git submodule`)
**Purpose**: Manage git submodules
**libgit2 API**: `git_submodule_*()`
**Use Cases**:
- Dependency management
- Monorepo structures
**Return**: Varies by operation
**Complexity**: High

```erlang
git:submodule_list(Repo) -> [SubmoduleInfo]
git:submodule_update(Repo, Name) -> ok | {error, Reason}
```

#### 15. **Note** (`git notes`)
**Purpose**: Add notes to commits
**libgit2 API**: `git_note_*()`
**Use Cases**:
- Metadata attachment to commits
- Review notes
- Changelog generation
**Return**: Varies
**Complexity**: Medium

#### 16. **Worktree** (`git worktree`)
**Purpose**: Create linked working directories
**libgit2 API**: Limited/no direct support in older libgit2
**Use Cases**:
- Parallel branch work
- CI/CD multi-branch builds
**Return**: Varies
**Complexity**: High (platform-dependent)

#### 17. **Attributes** (`git attributes`)
**Purpose**: Define file attributes (.gitattributes)
**Use Cases**:
- Line-ending normalization
- Merge strategy configuration
**Complexity**: Low

```erlang
git:attribute_get(Repo, Path, AttrName) -> {ok, Value} | {error, Reason}
```

---

## Implementation Priority Recommendation

### Phase 1 (Highest Impact, Lower Complexity)
1. **Blame** - Very useful for code analysis, moderate complexity
2. **Remove/Move** - Common file operations, low complexity
3. **Describe** - Valuable for versioning, low complexity
4. **Cherry-pick** - Common operation, medium complexity
5. **Reflog** - Useful debugging tool, low complexity

### Phase 2 (High Impact, Higher Complexity)
1. **Diff** - Complex but very valuable
2. **Merge** - Critical for automation
3. **Revert** - Safe alternative to reset
4. **Rebase** - Important for history management
5. **Stash** - Useful workflow feature

### Phase 3 (Specialized Operations)
1. **Verify** - Security feature
2. **Submodules** - For monorepo support
3. **Notes** - Metadata management
4. **Clean** - Maintenance operation
5. **Worktree** - Advanced parallel development

---

## Implementation Considerations

### API Design Patterns
- Follow existing egit patterns: binary/string inputs, map/tuple outputs
- Support Erlang/Elixir with flexible input types
- Use option lists for flexible configuration
- Return `ok` or `{error, Reason}` for side effects
- Return maps/tuples for data retrieval

### Error Handling
- Gracefully handle merge/rebase conflicts
- Provide detailed error messages from libgit2
- Support conflict resolution workflows

### Performance Notes
- Diff can be expensive for large repos - consider streaming
- Merge/rebase operations modify working directory - mark as DIRTY_JOB_IO_BOUND
- Blame can be memory-intensive

### Testing Requirements
- Create test repository fixtures
- Test success and failure paths
- Test conflict scenarios for merge/rebase/cherry-pick
- Validate error messages

---

## Missing Utilities (Non-Git Operations)

### 18. **Get Current Branch**
**libgit2 API**: `git_repository_head()`, `git_reference_shorthand()`
**Complexity**: Low

```erlang
git:current_branch(Repo) -> {ok, BranchName} | {error, Reason}
```

### 19. **Get Repository Info**
**Purpose**: Get path, is_bare, is_shallow, etc.
**libgit2 API**: `git_repository_*()`
**Complexity**: Low

```erlang
git:repo_info(Repo) -> #{path => ..., is_bare => ..., is_shallow => ...}
```

### 20. **Get All References**
**Purpose**: List all refs (branches, tags, remotes)
**libgit2 API**: `git_reference_iterator_*()`
**Complexity**: Low

```erlang
git:all_refs(Repo) -> [binary()] % "refs/heads/main", "refs/tags/v1.0", etc.
```

---

## Summary

**Most Recommended Quick Wins**: blame, describe, remove, move, reflog
**Most Impactful Medium-term**: diff, merge, revert
**Most Complex but Valuable**: rebase, stash, cherry-pick

Starting with the Phase 1 list would provide ~80% of commonly-used missing functionality with reasonable implementation effort.
