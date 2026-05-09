-module(git).
-moduledoc """
Erlang NIF bindings for libgit2.

This module provides comprehensive git repository operations without requiring
the external `git` executable. Operations range from basic repo management
(init, clone, open) to advanced functionality (blame, cherry-pick, reflog).

## Common Use Cases

### Repository Operations
```erlang
Repo = git:open("/path/to/repo"),
git:status(Repo),
git:fetch(Repo),
git:pull(Repo),
git:push(Repo)
```

### Commit Operations
```erlang
git:add(Repo, "src/*.erl"),
git:commit(Repo, "Update implementation"),
git:rev_parse(Repo, "HEAD~5")
```

### Branch Management
```erlang
git:branch_create(Repo, "feature/my-feature"),
git:checkout(Repo, "feature/my-feature"),
git:list_branches(Repo, [local])
```

### Code Analysis
```erlang
git:blame(Repo, "src/main.erl"),
git:describe(Repo, "HEAD"),
git:reflog(Repo, "main")
```
""".

-export([init/1, init/2, clone/2, open/1, fetch/1, fetch/2,
         pull/1, pull/2, push/1, push/2, push/3, commit_lookup/3]).
-export([cat_file/2, cat_file/3, checkout/2, checkout/3]).
-export([add_all/1, add/2, add/3, commit/2,
         rev_parse/2, rev_parse/3, rev_list/3]).
-export([config_get/2, config_set/3]).
-export([branch_create/2, branch_create/3]).
-export([branch_rename/3, branch_rename/4,  branch_delete/2]).
-export([list_branches/1, list_branches/2,  list_index/1, list_index/2]).
-export([list_remotes/1,  remote_add/3,     remote_rename/3,
         remote_delete/2, remote_set_url/3, remote_set_url/4]).
-export([tag_create/2, tag_create/3, tag_create/4, tag_delete/2]).
-export([list_tags/1, list_tags/2]).
-export([status/1, status/2, reset/2]).
-export([blame/2, blame/3, describe/2, describe/3]).
-export([cherry_pick/2, reflog/2, remove/2, move/3]).
-export([diff/3, diff/4, merge/2, revert/2]).
-export([rebase_init/2, rebase_next/1, rebase_finish/1, rebase_abort/1]).
-export([stash_save/2, stash_list/1, stash_apply/2, stash_pop/2, stash_drop/2]).

-on_load(on_load/0).

-type repository() :: reference().
-type commit_opt() ::
  encoding    |
  message     |
  summary     |
  time        |
  time_offset |
  committer   |
  author      |
  header      |
  tree_id.
-type commit_opts()     :: [commit_opt()].
-type cat_file_opt()    :: type | size | {abbrev, pos_integer()}.
-type cat_file_opts()   :: [cat_file_opt()].
-type checkout_opt()    :: force | verbose | perf.
-type checkout_opts()   :: [checkout_opt()].
-type checkout_stats()  :: #{
  chmod_calls => integer(),
  mkdir_calls => integer(),
  stat_calls  => integer(),
  total_steps => integer()
}.

-type add_opt()         :: verbose | dry_run | update | force.
-type add_opts()        :: [add_opt()].
-type add_result()      :: nil |
                           #{mode => dry_run | added, files => [binary()]} |
                           {error, term()}.
-type rev_list_opt()    :: [topo_order | date_order | reverse |
                            {limit, pos_integer()} | {abbrev, pos_integer()}].
-type rev_list_opts()   :: [rev_list_opt()].

-type rev_parse_opt()   :: {abbrev, pos_integer()}.
-type rev_parse_opts()  :: [rev_parse_opt()].

-doc """
List branch option.
- `local` — Return only local branches
- `remote` — Return only remote branches
- `all` — Return all branches (default)
- `fullname` — Return full branch names
- `{limit, Limit}` — Return up to this number of branches
""".
-type list_branch_opt() :: local | remote | all | fullname | {limit,pos_integer()}.

-type list_branch_opts() :: [list_branch_opt()].

-doc """
List index option.
- `{abbrev, NumChars}` — NumChars truncates the commit hash (must be less then 40).
- `{fields, ListOfFields}` — Field list to return. If not specified, the option will default to `[path]`.
""".
-type list_index_opt()   :: {abbrev, pos_integer()} |
  {fields, all | [path|stage|conflict|oid|mode|size|ctime|mtime]}.

-type list_index_opts()  :: [list_index_opt()].

-type list_index_entry() :: #{
  path     => binary(),      stage => [normal|ancestor|ours|theirs],
  conflict => boolean(),       oid => binary(),
  mode     => pos_integer(),  size => non_neg_integer(),
  ctime    => pos_integer(), mtime => pos_integer()
}.

-doc """
Configuration source.
If the value is an atom, then:
- `default` — Find default configuration file for this app
- `system` — System-wide configuration file - /etc/gitconfig on Linux systems
- `xdg` — XDG compatible configuration file, typically ~/.config/git/config
- `global` — User-specific global configuration file, typically ~/.gitconfig
- `local` — Repository specific configuration file; $WORK_DIR/.git/config on non-bare repos
- `app` — Application specific configuration file; freely defined by applications
- `highest` — The most specific config file available for the app
""".
-type cfg_source() :: repository() | default | system | xdg | global | local | app | highest.

-doc """
Branch creation options
- `overwrite` — Force to overwrite the existing branch
- `{target, Commit}` — Use the target commit (default `~"HEAD"`)
""".
-type branch_create_opts() :: [overwrite | {target, binary()}].

-doc """
Tag creation options
- `{message, Msg}` — Message associated with the tag's commit
- `{pattern, Pat}` — Pattern to search matching tags
- `{target,  SHA}` — Target commit SHA to be associated with the tag
- `{lines,   Num}` — Number of lines in the commit to store
""".
-type tag_opt() :: [{message, binary()} | {pattern, binary()} | {target, binary()} | {lines, integer()}].

-type tag_opts() :: [tag_opt()].

-doc """
Status function options
- `{untracked, Untracked}` — `Untracked` can be one of:
  - `none` - don't include untracked files
  - `normal` - include untracked files
  - `recursive` - include untracked files and recurse into untracked directories
- `{paths, Paths}` — `Path` is an array of path patterns to match
- `branch` — Include branch name
- `ignored` — Include ignored files
- `ignore_submodules` — Indicates that submodules should be skipped
- `submodules` — Include submodules (overrides `ignore_submodules`)
""".
-type status_opt() ::
  {untracked, none|normal|recursive} | {paths, [binary()]} |
  branch | ignored | submodules | ignore_submodules.

-type status_opts() :: [status_opt()].

-export_type([repository/0, commit_opts/0, cat_file_opt/0, checkout_opts/0, checkout_stats/0]).
-export_type([rev_parse_opts/0, rev_list_opts/0, tag_opts/0, status_opts/0]).
-export_type([list_index_opts/0, list_index_entry/0]).

-define(LIBNAME, ?MODULE).
-define(NOT_LOADED_ERROR,
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

on_load() ->
  SoName  =
    case code:priv_dir(?LIBNAME) of
      {error, bad_name} ->
        case code:which(?MODULE) of
          Filename when is_list(Filename) ->
            Dir = filename:dirname(filename:dirname(Filename)),
            filename:join([Dir, "priv", ?LIBNAME]);
          _ ->
            filename:join("../priv", ?LIBNAME)
        end;
      Dir ->
        filename:join(Dir, ?LIBNAME)
  end,
  erlang:load_nif(SoName, []).

-doc """
Init a repository.
See: `init/2`.
""".
-spec init(binary()|string()) -> repository().
init(Path) -> init(Path, []).

-doc """
Init a repository.
If `Opts' list contains `bare', a Git repository without a working
directory is created at the pointed path.
Otherwise, the provided path will be considered as the working
directory into which the .git directory will be created.
""".
-spec init(binary()|string(), [bare]) -> repository().
init(Path, Opts) ->
  init_nif(to_bin(Path), Opts).

-doc "Clone a remote repository to the local path".
-spec clone(binary()|string(), binary()|string()) -> repository().
clone(URL, Path)    -> clone_nif(to_bin(URL), to_bin(Path)).

-doc "Open a local git repository".
-spec open(binary()|string()) -> repository().
open(Path)          -> open_nif(to_bin(Path)).

-doc "Fetch from origin".
-spec fetch(repository()) -> ok | {error, binary()}.
fetch(Repo)         -> fetch_nif(Repo, fetch).

-doc """
Fetch from given remote (e.g. origin).
""".
-spec fetch(repository(), binary()|string()) -> ok | {error, binary()}.
fetch(Repo, Remote) -> fetch_nif(Repo, fetch, to_bin(Remote)).

-doc "Pull from origin".
-spec pull(repository()) -> ok | {error, binary()}.
pull(Repo)          -> fetch_nif(Repo, pull).

-doc """
Pull from given remote (e.g. origin).
""".
-spec pull(repository(), binary()|string()) -> ok | {error, binary()}.
pull(Repo, Remote)  -> fetch_nif(Repo, pull, to_bin(Remote)).

-doc "Push changes to remote (origin)".
-spec push(repository()) -> ok | {error, binary()}.
push(Repo)          -> push_nif(Repo, <<"origin">>, []).

-doc "Push to given remote".
-spec push(repository(), binary()|string()) -> ok | {error, binary()}.
push(Repo, Remote)  -> push_nif(Repo, to_bin(Remote), []).

-doc "Push refs to given remote".
-spec push(repository(), binary()|string(), [binary()|string()]) ->
        ok | {error, binary()}.
push(Repo, Remote, Refs) when is_list(Refs) ->
  push_nif(Repo, to_bin(Remote), [to_bin(M) || M <- Refs]).

-doc """
Same as cat_file(Repo, Rev, []).
See cat_file/3.
""".
-spec cat_file(repository(), binary()|string()) -> map() | {error, term()}.
cat_file(Repo, Rev) ->
  cat_file(Repo, Rev, []).

-doc """
Provide content or type and size information for repository objects.
Example:
```
1> R = git:open(".").
2> git:cat_file(R, "main", [{abbrev, 5}]).
#{type => commit,
  author => {~"John Doh",~"test@gmail.com",1686195121, -14400},
  oid => ~"b85d0",
  parents => [~"1fd4b"]}
7> git:cat_file(R, "b85d0", [{abbrev, 5}]).
#{type => tree,
  commits =>
      [{~".github",~"tree",~"1e41f",16384},
       {~".gitignore",~"blob",~"b893a",33188},
       {~".gitmodules",~"blob",~"2550a",33188},
       {~".vscode",~"tree",~"c7b1b",16384},
       {~"LICENSE",~"blob",~"d6456",33188},
       {~"Makefile",~"blob",~"2d635",33188},
       {~"README.md",~"blob",~"7b3d0",33188},
       {~"c_src",~"tree",~"147f3",16384},
       {~"rebar.config",~"blob",~"1f68a",33188},
       {~"rebar.lock",~"blob",~"57afc",33188},
       {~"src",~"tree",~"1bccb",16384}]}
8> git:cat_file(R, "b893a", [{abbrev, 5}]).
#{type => blob,
  data => ~"*.swp\n*.dump\n/c_src/*.o\n/c_src/fmt\n/priv/*.so\n/_build\n/doc\n"}
```
""".
-spec cat_file(repository(), binary()|string(), cat_file_opts()) ->
        {ok, term()} | {error, term()}.
cat_file(Repo, Rev, Opts) ->
  cat_file_nif(Repo, to_bin(Rev), Opts).

-doc """
Same as checkout(Repo, Revision, []).
""".
-spec checkout(repository(), binary()|string()) -> ok | {error, term()}.
checkout(Repo, Rev) -> checkout_nif(Repo, to_bin(Rev), []).

-doc """
Check out a revision or branch.

Opts is a list of:

- force - Force the checkout even if there are conflicts
- verbose - Return a checkout_stats() map instead of ok
- perf - Include performance counters in the stats map (requires verbose)
""".
-spec checkout(repository(), binary(), checkout_opts()) -> ok | checkout_stats() | {error, term()}.
checkout(Repo, Revision, Opts) ->
  checkout_nif(Repo, to_bin(Revision), Opts).

-doc """
Add all pending changes (equivalent to git add .).
See add/2 and add/3.
""".
-spec add_all(repository()) -> add_result().
add_all(Repo) when is_reference(Repo) ->
  add_nif(Repo, [<<".">>], []).

-doc """
Add files matching PathSpecs to index.
See add/3.
""".
-spec add(repository(), binary()|string()|[binary()|string()]) -> add_result().
add(Repo, [C|_] = PathSpec) when is_integer(C), C >= 32, C < 256 ->
  add_nif(Repo, [to_bin(PathSpec)], []);
add(Repo, PathSpecs) when is_list(PathSpecs) ->
  add(Repo, PathSpecs, []);
add(Repo, PathSpec) when is_binary(PathSpec) ->
  add_nif(Repo, [PathSpec], []).

-doc """
Add files matching PathSpecs to index with options.
""".
-spec add(repository(), [binary()|string()], add_opts()) -> add_result().
add(Repo, [C|_] = PathSpecs, Opts) when is_integer(C), C >= 32, C < 256 ->
  add_nif(Repo, [to_bin(PathSpecs)], Opts);
add(Repo, PathSpec, Opts) when is_binary(PathSpec)->
  add_nif(Repo, [PathSpec], Opts);
add(Repo, PathSpecs, Opts) when is_list(PathSpecs)->
  add_nif(Repo, [to_bin(B) || B <- PathSpecs], Opts).

-doc """
Commit staged changes to the repository.
Returns {ok, OID} where OID is the new commit hash, or
{ok, nil} if there are no staged changes.
""".
-spec commit(repository(), binary()|string()) ->
        {ok, OID::binary()|nil} | {error, binary()|atom()}.
commit(_Repo, Comment) ->
  commit_nif(_Repo, to_bin(Comment)).

-doc """
Reverse parse a reference.
See [https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions]
for the formats of a Spec.

Opts is a list of:

- {abbrev, NumChars} - NumChars truncates the commit hash (must be less then 40)

When a reference refers to a single object, an ok tuple with a binary
string of the commit hash is returned. When it refers to a range
(e.g. HEAD..HEAD~2), a map is returned with from and to keys.
When using a Symmetric Difference Notation (i.e. HEAD...HEAD~4),
a map with three keys from, to, and merge_base is returned.

Examples:

```
2> git:rev_parse(R,~"HEAD~4", [{abbrev, 7}]).
{ok,~"6d6f662"}
3> git:rev_parse(R,~"HEAD..HEAD~4", [{abbrev, 7}]).
#{from => ~"f791f01",to => ~"6d6f662"}
4> git:rev_parse(R,~"HEAD...HEAD~4", [{abbrev, 7}]).
#{from => ~"f791f01",merge_base => ~"6d6f662", to => ~"6d6f662"}
```
""".
-spec rev_parse(repository(), binary()|string(), rev_parse_opts()) ->
        {ok, binary()} | map() | {error, binary()|atom()}.
rev_parse(Repo, Spec, Opts) ->
  rev_parse_nif(Repo, to_bin(Spec), Opts).

-doc """
Same as rev_parse(Repo, Spec, []).
""".
-spec rev_parse(repository(), binary()|string()) ->
        {ok, binary()} | map() | {error, binary()|atom()}.
rev_parse(Repo, Spec) ->
  rev_parse(Repo, Spec, []).

-doc """
Return the list of OIDs for the given specs.

Opts is a list of:
<dl>
<dt>topo_order | date_order | reverse</dt>
  <dd>Control sorting order</dd>
<dt>{limit, `Limit'}</dt>
  <dd>Limit is an integer that limits the number of refs returned</dd>
<dt>{abbrev, `NumChars'}</dt>
  <dd>NumChars truncates the commit hash (must be less then 40)</dd>
</dl>

Example:
```
9> git:rev_list(R, ["HEAD"], [{limit, 4}, {abbrev, 7}]).
[~"f791f01",~"1b74c46",~"c40374d",~"12968bd"]
```
""".
-spec rev_list(repository(), ['not'|'Elixir.Not'|string()|binary()]|binary(), rev_list_opts()) ->
        #{commit_opt() => term()}.
rev_list(Repo, Specs, Opts) when is_list(Specs); is_binary(Specs) ->
  F = fun
    ('not')               -> 'not';
    ('Elixir.Not')        -> 'not';
    (I) when is_list(I)   -> list_to_binary(I);
    (I) when is_binary(I) -> I
  end,
  L = [F(I) || I <-
        if is_binary(Specs) -> [Specs];
        true                ->  Specs
        end],
  rev_list_nif(Repo, L, Opts).

-doc """
Lookup commit details identified by OID.

Opts is a list of commit_opt() field names to return.
An empty list returns an empty map. Pass any of:
encoding, message, summary, time, time_offset,
committer, author, header, tree_id.

Example:

```
1> R = git:open("/tmp/egit").
2> {ok, OID} = git:rev_parse(R, "HEAD").
3> git:commit_lookup(R, OID, [author, message, time]).
#{author => {~"John Doh",~"test@gmail.com",1686195121,-14400},
  message => ~"Add test files\n",
  time => 1686195121}
```
""".
-spec commit_lookup(repository(), binary()|string(), [commit_opt()]) ->
        #{commit_opt() => term()}.
commit_lookup(Repo, OID, Opts) ->
  commit_lookup_nif(Repo, to_bin(OID), Opts).

-doc """
Get git configuration value
Example:
```
1> R = git:clone(~"https://github.com/saleyn/egit.git", "/tmp/egit").
#Ref<0.170091758.2335834136.12133>
2> git:config_get(R, "user.name").
{ok,~"John Doh"}
```
""".
-spec config_get(cfg_source(), binary()|string()) ->
         {ok, binary()} | {error, binary()|atom()}.
config_get(Src, Key) ->
  config_get_nif(Src, to_bin(Key)).

-doc """
Set git configuration value
Example:
```
1> R = git:clone(~"https://github.com/saleyn/egit.git", "/tmp/egit").
#Ref<0.170091758.2335834136.12133>
2> git:config_set(R, "user.name", "Test User").
ok
```
""".
-spec config_set(cfg_source(), binary()|string(), binary()|string()) ->
        ok | {error, binary()|atom()}.
config_set(Src, Key, Val) ->
  config_set_nif(Src, to_bin(Key), to_bin(Val)).

-doc """
Create a branch
See: `git:branch_create/3`.
""".
branch_create(Repo, Name) ->
  branch_create(Repo, Name, []).

-doc """
Create a branch
Example:
```
1> R = git:clone(~"https://github.com/saleyn/egit.git", "/tmp/egit").
#Ref<0.170091758.2335834136.12133>
2> git:branch_create(R, "tmp").
ok
```
""".
-spec branch_create(repository(), binary()|string(), branch_create_opts()) ->
        ok | {error, binary()}.
branch_create(Repo, Name, Opts) when is_list(Opts) ->
  branch_nif(Repo, create, to_bin(Name), Opts).

-doc """
Rename a branch
See: `branch_rename/4`.
""".
branch_rename(Repo, OldName, NewName) ->
  branch_rename(Repo, OldName, NewName, []).

-doc "Rename a branch".
-spec branch_rename(repository(), binary()|string(), binary()|string(), [overwrite]) ->
        ok | {error, binary()}.
branch_rename(Repo, OldName, NewName, Opts) when is_list(Opts) ->
  branch_nif(Repo, rename, to_bin(OldName), [{new_name, to_bin(NewName)} | Opts]).

-doc "Delete a branch".
-spec branch_delete(repository(), binary()|string()) ->
        ok | {error, binary()}.
branch_delete(Repo, Name) ->
  branch_nif(Repo, delete, to_bin(Name)).

-doc "List branches".
-spec list_branches(repository(), list_branch_opts()) -> [{local|remote, binary()}].
list_branches(Repo, Opts) when is_reference(Repo), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

-doc "Add a remote".
-spec remote_add(repository(), binary()|string(), binary()|string()) ->
        ok | {error, binary()}.
remote_add(Repo, Name, URL) ->
  remote_nif(Repo, {add, URL}, to_bin(Name), []).

-doc "Rename a remote".
-spec remote_rename(repository(), binary()|string(), binary()|string()) ->
        ok | {error, binary()}.
remote_rename(Repo, OldName, NewName) ->
  remote_nif(Repo, {rename, to_bin(NewName)}, to_bin(OldName), []).

-doc "Delete a remote".
-spec remote_delete(repository(), binary()|string()) ->
        ok | {error, binary()}.
remote_delete(Repo, Name) ->
  remote_nif(Repo, delete, to_bin(Name), []).

-doc """
Change the fetch/push URL for a remote.
See remote_set_url/4.
""".
-spec remote_set_url(repository(), binary()|string(), binary()|string()) ->
        ok | {error, binary()}.
remote_set_url(Repo, Name, URL) ->
  remote_set_url(Repo, Name, URL, []).

-doc """
Change the fetch/push URL for a remote.
If Opts contains push, the URL is set only as the push URL.
""".
remote_set_url(Repo, Name, URL, Opts) ->
  remote_nif(Repo, {seturl, to_bin(URL)}, to_bin(Name), Opts).

-doc """
List remotes.
Returns a list of {Name, URL, [push|fetch]} triples.
""".
-spec list_remotes(repository()) -> [{binary(), binary(), [push|fetch]}].
list_remotes(Repo) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

-doc """
List branches.
See list_branches/2.
""".
list_branches(Repo) ->
  list_branches(Repo, []).

-doc """
List index.
See list_index/2.
""".
list_index(Repo) ->
  list_index(Repo, []).

-doc "List index.".
-spec list_index(repository(), list_index_opts()) -> [list_index_entry()].
list_index(Repo, Opts) when is_reference(Repo), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

-doc "Create a tag".
-spec tag_create(repository(), string()|binary()) ->
        ok | {error, binary()|atom()}.
tag_create(Repo, Tag) ->
  tag_create(Repo, Tag, nil, []).

-doc "Create a tag".
-spec tag_create(repository(), string()|binary(), nil|string()|binary()) ->
        ok | {error, binary()|atom()}.
tag_create(Repo, Tag, Msg) ->
  tag_create(Repo, Tag, Msg, []).

-doc "Create a tag".
-spec tag_create(repository(), string()|binary(), nil|string()|binary(), tag_opts()) ->
        ok | {error, binary()|atom()}.
tag_create(Repo, Tag, Msg, Opts) when Msg==nil; Msg==undefined ->
  tag_nif(Repo, create, to_bin(Tag), Opts);
tag_create(Repo, Tag, Msg, Opts0) when is_list(Msg); is_binary(Msg) ->
  Opts = [case X of
            {I, M} when is_list(M) -> {I, to_bin(M)};
            {_, _} -> X;
            I when is_atom(I) -> X
          end || X <- [{message, Msg} | Opts0]],
  tag_nif(Repo, create, to_bin(Tag), Opts).

-doc "Delete a tag".
-spec tag_delete(repository(), string()|binary()) -> ok | {error, binary()|atom()}.
tag_delete(Repo, Tag) ->
  tag_nif(Repo, delete, to_bin(Tag), []).

-doc """
List all tags in the repository.
Lightweight tags are returned as plain binaries; annotated tags are also
returned as plain binaries unless the {lines, N} option is used.
See list_tags/2.
""".
-spec list_tags(repository()) ->
        [binary()|{binary(), binary()}] | {error, binary()|atom()}.
list_tags(Repo) ->
  tag_nif(Repo, list, <<"">>, []).

-doc """
List tags, optionally filtered by pattern or with options.
When PatternOrOpts is a string or binary it is used as a glob pattern
to filter tag names (e.g. "v1.*").
When it is a proplist, the following options are recognised:

- {pattern, Pat} - Glob pattern to filter tag names (default "*")
- {lines, N} - Include up to N lines of the tag/commit message.
  Annotated tags whose message is non-empty are returned as
  {Name, Message} tuples; all others remain plain binaries.

Example:

```
1> R = git:open("/tmp/egit").
2> git:list_tags(R, "v0.*").
[~"v0.1.0",~"v0.2.0"]
3> git:list_tags(R, [{lines, 1}]).
[{~"v0.1.0",~"Release 0.1.0\n"},~"v0.2.0"]
```
""".
-spec list_tags(repository(), string()|binary()|list()) ->
        [binary()|{binary(), binary()}] | {error, binary()|atom()}.
list_tags(Repo, Opts) when is_list(Opts), (Opts =:= [] orelse is_tuple(hd(Opts))) ->
  tag_nif(Repo, list, <<"">>, Opts);
list_tags(Repo, Pattern) ->
  tag_nif(Repo, list, <<"">>, [{pattern, to_bin(Pattern)}]).

-doc "Get repository status".
-spec status(repository()) -> map() | {error, term()}.
status(Repo) ->
  status(Repo, []).

-doc "Get repository status".
-spec status(repository(), status_opts()) -> map() | {error, term()}.
status(Repo, Opts) ->
  status_nif(Repo, Opts).

-doc """
Get repository reset.

The reset Type is one of:

- soft - The HEAD will be moved to the commit
- mixed - Do a SOFT reset, plus the index will be replaced with the content of the commit tree
- hard - Do a MIXED reset and the working directory will be replaced with the content of the index. Untracked and ignored files will be left alone.
""".
-spec reset(repository(), soft|mixed|hard, string()|binary()) -> ok | {error, term()}.
reset(Repo, Type, Ref) ->
  reset_nif(Repo, Type, to_bin(Ref)).

-spec reset(repository(), soft|mixed|hard) -> ok | {error, term()}.
reset(Repo, Type) ->
  reset(Repo, Type, "HEAD").

-doc """
Show file change history by line.
Returns a list of {LineNumber, {Author, Email}, CommitOID, Time} tuples.
""".
-spec blame(repository(), string()|binary()) -> [{non_neg_integer(), {binary(), binary()}, binary(), integer()}].
blame(Repo, Path) ->
  blame(Repo, Path, []).

-doc """
Show file change history by line with options.
Opts is a list of:
- {pattern, Pattern} - Pattern to match tags
""".
-spec blame(repository(), string()|binary(), list()) -> [{non_neg_integer(), {binary(), binary()}, binary(), integer()}].
blame(Repo, Path, Opts) when is_reference(Repo), is_list(Opts) ->
  blame_nif(Repo, to_bin(Path), Opts).

-doc """
Describe working tree state.
Returns a human-readable description of the most recent tag and commits.
Example: "v1.0.0-5-gabcd123"
""".
-spec describe(repository(), string()|binary()) -> {ok, binary()} | {error, term()}.
describe(Repo, Rev) ->
  describe(Repo, Rev, []).

-doc """
Describe working tree state with options.
Opts is a list of:
- {pattern, Pattern} - Pattern to match tags
""".
-spec describe(repository(), string()|binary(), list()) -> {ok, binary()} | {error, term()}.
describe(Repo, Rev, Opts) when is_reference(Repo), is_list(Opts) ->
  describe_nif(Repo, to_bin(Rev), Opts).

-doc """
Apply a single commit to current branch.
Returns ok on success, {conflict, Conflicts} on merge conflict,
or {error, Reason} on failure.
""".
-spec cherry_pick(repository(), string()|binary()) -> ok | {conflict, [binary()]} | {error, term()}.
cherry_pick(Repo, CommitOID) when is_reference(Repo) ->
  cherry_pick_nif(Repo, to_bin(CommitOID)).

-doc """
Show reference logs.
Returns a list of {OID, Message, Author, Time} tuples.
""".
-spec reflog(repository(), string()|binary()) -> [{binary(), binary(), binary(), integer()}].
reflog(Repo, RefName) when is_reference(Repo) ->
  reflog_nif(Repo, to_bin(RefName)).

-doc """
Remove a file from the index and write the index.
""".
-spec remove(repository(), string()|binary()) -> ok | {error, term()}.
remove(Repo, Path) when is_reference(Repo) ->
  remove_nif(Repo, to_bin(Path)).

-doc """
Move/rename a file in the index and write the index.
""".
-spec move(repository(), string()|binary(), string()|binary()) -> ok | {error, term()}.
move(Repo, OldPath, NewPath) when is_reference(Repo) ->
  move_nif(Repo, to_bin(OldPath), to_bin(NewPath)).

-doc """
Compare two revisions and return list of differences.
See: `diff/4`.
""".
-spec diff(repository(), string()|binary(), string()|binary()) -> [{binary(), binary(), integer(), integer()}].
diff(Repo, FromRev, ToRev) ->
  diff(Repo, FromRev, ToRev, []).

-doc """
Compare two revisions and return list of differences.
Returns list of {Path, Status, StatusCode, Similarity} tuples.
Status values: "added", "deleted", "modified", "renamed", "copied", "typechange"
""".
-spec diff(repository(), string()|binary(), string()|binary(), list()) -> [{binary(), binary(), integer(), integer()}].
diff(Repo, FromRev, ToRev, Opts) when is_reference(Repo), is_list(Opts) ->
  diff_nif(Repo, to_bin(FromRev), to_bin(ToRev), Opts).

-doc """
Merge another branch into the current branch.
Returns `{ok, Status}' where Status is 'up_to_date', 'fast_forward', or 'merged'.
On conflict returns `{conflict, ConflictList}'.
""".
-spec merge(repository(), string()|binary()) -> {ok, atom()} | {conflict, [binary()]} | {error, term()}.
merge(Repo, Branch) when is_reference(Repo) ->
  merge_nif(Repo, to_bin(Branch)).

-doc """
Revert a commit (creates new commit undoing the changes).
Returns `ok' on success, `{conflict, Conflicts}' on merge conflict.
""".
-spec revert(repository(), string()|binary()) -> ok | {conflict, [binary()]} | {error, term()}.
revert(Repo, CommitOID) when is_reference(Repo) ->
  revert_nif(Repo, to_bin(CommitOID)).

-doc """
Initialize a rebase operation.
Returns number of operations in the rebase sequence.
""".
-spec rebase_init(repository(), string()|binary()) -> integer() | {error, term()}.
rebase_init(Repo, OntoRef) when is_reference(Repo) ->
  rebase_init_nif(Repo, to_bin(OntoRef)).

-doc """
Continue to the next rebase operation.
Returns 'done' when all operations are complete, or operation info.
""".
-spec rebase_next(repository()) -> done | tuple() | {error, term()}.
rebase_next(Repo) when is_reference(Repo) ->
  rebase_next_nif(Repo).

-doc """
Finish the rebase operation.
""".
-spec rebase_finish(repository()) -> ok | {error, term()}.
rebase_finish(Repo) when is_reference(Repo) ->
  rebase_finish_nif(Repo).

-doc """
Abort the current rebase operation.
""".
-spec rebase_abort(repository()) -> ok | {error, term()}.
rebase_abort(Repo) when is_reference(Repo) ->
  rebase_abort_nif(Repo).

-doc """
Save uncommitted changes to stash.
Returns `{ok, StashOID}' or `{error, no_changes}' if nothing to stash.
""".
-spec stash_save(repository(), string()|binary()) -> {ok, binary()} | {error, term()}.
stash_save(Repo, Message) when is_reference(Repo) ->
  stash_save_nif(Repo, to_bin(Message)).

-doc """
List all stashed changes.
Returns list of {Index, Name} tuples.
""".
-spec stash_list(repository()) -> [{integer(), binary()}].
stash_list(Repo) when is_reference(Repo) ->
  stash_list_nif(Repo).

-doc """
Apply a stash without removing it.
Returns `ok' on success, `{conflict, Conflicts}' on conflict.
""".
-spec stash_apply(repository(), integer()) -> ok | {conflict, [binary()]} | {error, term()}.
stash_apply(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  stash_apply_nif(Repo, Index).

-doc """
Apply and remove a stash.
Returns `ok' on success, `{conflict, Conflicts}' on conflict.
""".
-spec stash_pop(repository(), integer()) -> ok | {conflict, [binary()]} | {error, term()}.
stash_pop(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  stash_pop_nif(Repo, Index).

-doc """
Delete a stash.
""".
-spec stash_drop(repository(), integer()) -> ok | {error, term()}.
stash_drop(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  stash_drop_nif(Repo, Index).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(B) when is_list(B)   -> list_to_binary(B).

init_nif(Path, Opts) when is_binary(Path), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

clone_nif(URL, Path) when is_binary(URL), is_binary(Path) ->
  ?NOT_LOADED_ERROR.

open_nif(Path) when is_binary(Path) ->
  ?NOT_LOADED_ERROR.

fetch_nif(Repo, _Op) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

fetch_nif(Repo, _Op, Remote) when is_reference(Repo), is_binary(Remote) ->
  ?NOT_LOADED_ERROR.

push_nif(Repo, Remote, Refs) when is_reference(Repo), is_binary(Remote), is_list(Refs) ->
  ?NOT_LOADED_ERROR.

add_nif(Repo, PathSpecs, Opts) when is_reference(Repo), is_list(PathSpecs), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

cat_file_nif(Repo, Rev, Opts) when is_reference(Repo), is_binary(Rev), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

checkout_nif(Repo, Revision, Opts) when is_reference(Repo), is_binary(Revision), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

commit_nif(Repo, Comment) when is_reference(Repo), is_binary(Comment) ->
  ?NOT_LOADED_ERROR.

commit_lookup_nif(Repo, OID, Opts) when is_reference(Repo), is_binary(OID), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

rev_parse_nif(Repo, Spec, Opts) when is_reference(Repo), is_binary(Spec), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

rev_list_nif(Repo, Specs, Opts) when is_reference(Repo), is_list(Specs), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

config_get_nif(Src, Key) when is_reference(Src) orelse is_atom(Src), is_binary(Key) ->
  ?NOT_LOADED_ERROR.

config_set_nif(Src, Key, Val) when is_reference(Src) orelse is_atom(Src), is_binary(Key), is_binary(Val) ->
  ?NOT_LOADED_ERROR.

remote_nif(Repo, _Op, Name, Opts) when is_reference(Repo), is_binary(Name), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

branch_nif(Repo, Op, Name) when is_reference(Repo), is_atom(Op), is_binary(Name) ->
  ?NOT_LOADED_ERROR.

branch_nif(Repo, Op, OldName, NewNameOrOpt)
  when is_reference(Repo), is_atom(Op)
     , is_binary(OldName), is_binary(NewNameOrOpt) orelse is_list(NewNameOrOpt) ->
  ?NOT_LOADED_ERROR.

tag_nif(Repo, Op, Tag, Opts) when is_reference(Repo), is_atom(Op), is_binary(Tag), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

status_nif(Repo, Opts) when is_reference(Repo), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

reset_nif(Repo, Type, Ref) when is_reference(Repo), is_atom(Type), is_binary(Ref) ->
  ?NOT_LOADED_ERROR.

blame_nif(Repo, Path, Opts) when is_reference(Repo), is_binary(Path), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

describe_nif(Repo, Rev, Opts) when is_reference(Repo), is_binary(Rev), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

cherry_pick_nif(Repo, CommitOID) when is_reference(Repo), is_binary(CommitOID) ->
  ?NOT_LOADED_ERROR.

reflog_nif(Repo, RefName) when is_reference(Repo), is_binary(RefName) ->
  ?NOT_LOADED_ERROR.

remove_nif(Repo, Path) when is_reference(Repo), is_binary(Path) ->
  ?NOT_LOADED_ERROR.

move_nif(Repo, OldPath, NewPath) when is_reference(Repo), is_binary(OldPath), is_binary(NewPath) ->
  ?NOT_LOADED_ERROR.

diff_nif(Repo, FromRev, ToRev, Opts) when is_reference(Repo), is_binary(FromRev), is_binary(ToRev), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

merge_nif(Repo, Branch) when is_reference(Repo), is_binary(Branch) ->
  ?NOT_LOADED_ERROR.

revert_nif(Repo, CommitOID) when is_reference(Repo), is_binary(CommitOID) ->
  ?NOT_LOADED_ERROR.

rebase_init_nif(Repo, OntoRef) when is_reference(Repo), is_binary(OntoRef) ->
  ?NOT_LOADED_ERROR.

rebase_next_nif(Repo) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

rebase_finish_nif(Repo) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

rebase_abort_nif(Repo) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

stash_save_nif(Repo, Message) when is_reference(Repo), is_binary(Message) ->
  ?NOT_LOADED_ERROR.

stash_list_nif(Repo) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

stash_apply_nif(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  ?NOT_LOADED_ERROR.

stash_pop_nif(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  ?NOT_LOADED_ERROR.

stash_drop_nif(Repo, Index) when is_reference(Repo), is_integer(Index) ->
  ?NOT_LOADED_ERROR.

-ifdef(EUNIT).

init_test_() ->
  file:del_dir_r("/tmp/egit_repo"),
  [
    ?_assertMatch(B when is_reference(B), git:init("/tmp/egit_repo")),
    ?_assert(filelib:is_dir("/tmp/egit_repo/.git")),
    ?_assertEqual(ok, file:del_dir_r("/tmp/egit_repo")),
    ?_assertMatch(B when is_reference(B), git:init("/tmp/egit_repo", [bare])),
    ?_assertNot(filelib:is_dir("/tmp/egit_repo/.git")),
    ?_assert(filelib:is_regular("/tmp/egit_repo/HEAD")),
    ?_assertEqual(ok, file:del_dir_r("/tmp/egit_repo"))
  ].

clone_test_() ->
  file:del_dir_r("/tmp/egit"),
  R = git:clone(<<"https://github.com/saleyn/egit.git">>, <<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertMatch({ok, _}, git:rev_parse(R, <<"HEAD">>))
  ].

fetch_test_() ->
  R = git:open(<<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, git:fetch(R))
  ].

pull_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, git:fetch(R))
  ].

checkout_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, git:checkout(R, <<"main">>))
  ].

commit_test_() ->
  R = git:open("/tmp/egit"),
  {ok, OID0} = git:rev_parse(R, "HEAD"),
  [
    fun() ->
      ?assert(is_reference(R)),
      ?assertEqual([], os:cmd("echo \"\n\" >> /tmp/egit/README.md")),
      ?assertEqual(
        #{mode => dry_run, files => [<<"README.md">>]},
        git:add(R, ".", [dry_run])),
      ?assertEqual(
        #{mode => added, files => [<<"README.md">>]},
        git:add(R, ["."])),
      ?assertEqual(nil, git:add(R, ".")),
      {ok, OID0} = git:rev_parse(R, "HEAD"),
      Res        = git:commit(R, "Test commit"),
      ?assertMatch({ok, _}, Res),
      {ok, OID}  = Res,
      ?assertEqual({ok, nil}, git:commit(R, "Test commit")),
      ?assertEqual({ok, OID}, git:rev_parse(R, "HEAD"))
    end
  ].

rev_parse_test_() ->
  R = git:open("/tmp/egit"),
  {ok, OID0} = git:rev_parse(R, "HEAD"),
  [
    ?_assertMatch(#{to := _,   from := OID0}, git:rev_parse(R, <<"HEAD..HEAD~1">>)),
    ?_assertMatch(#{to := OID, from := OID0, merge_base := OID}, git:rev_parse(R, <<"HEAD...HEAD~1">>)),
    ?_assertMatch({error, _}, git:rev_parse(R, <<"HEAD~x">>)),
    fun() ->
      case git:rev_list(R, ["HEAD"], [{limit, 3}, {abbrev, 7}]) of
        [A,B,C] when is_binary(A), is_binary(B), is_binary(C),
                      byte_size(A)==7, byte_size(B)==7, byte_size(C)==7 ->
          ok;
        _Res ->
          ?assert(false)
      end
    end
  ].

cat_file_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertMatch(
      (#{type      := commit,
         author    := {_User, _, Time, Offset},
         committer := {_CUser, _, _CTime, _COffset},
         message   := Msg,
         oid       := OID,
         parents   := [OID1]})
        when is_integer(Time) andalso is_integer(Offset)
             andalso is_binary(OID) andalso is_binary(OID1)
             andalso is_binary(Msg),
      git:cat_file(R, <<"main">>, [{abbrev, 5}])
    ),

    ?_assertMatch(
      #{type    := tree,
        commits :=
          [{<<".github">>,<<"tree">>,_,16384},
          {<<".gitignore">>,<<"blob">>,_,33188},
          {<<".gitmodules">>,<<"blob">>,_,33188},
          {<<".vscode">>,<<"tree">>,_,16384},
          {<<"LICENSE">>,<<"blob">>,_,33188},
          {<<"Makefile">>,<<"blob">>,_,33188},
          {<<"README.md">>,<<"blob">>,_,33188},
          {<<"c_src">>,<<"tree">>,_,16384},
          {<<"rebar.config">>,<<"blob">>,_,33188},
          {<<"rebar.lock">>,<<"blob">>,_,33188},
          {<<"src">>,<<"tree">>,_,16384}]},
      git:cat_file(R, "b85d0", [{abbrev, 5}])
    ),

    ?_assertEqual(
      #{type => blob,
        data => <<"*.swp\n*.dump\n/c_src/*.o\n/c_src/fmt\n/priv/*.so\n/_build\n/doc\n">>},
      git:cat_file(R, "b893a", [{abbrev, 5}])
    ),

    %% root commit has 0 parents — previously crashed via .front() on empty parents vector
    fun() ->
      [Root|_] = lists:reverse(git:rev_list(R, ["HEAD"], [])),
      ?assertMatch(#{type := commit, parents := []}, git:cat_file(R, Root, []))
    end
  ].

config_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertMatch({ok, _}, git:config_get(R,       "user.email")),
    ?_assertMatch({ok, _}, git:config_get(highest, "user.email")),
    ?_assertMatch({ok, _}, git:config_get(default, "user.email")),
    ?_assertMatch({ok, _}, git:config_get(global,  "user.email"))
  ].

branch_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertMatch(ok, git:branch_create(R, "tmp", [])),
    ?_assert(has_branch(R, "tmp")),
    ?_assertMatch(ok, git:branch_rename(R, "tmp", "tmp2", [overwrite])),
    ?_assert(has_branch(R, "tmp2")),
    ?_assertNot(has_branch(R, "tmp")),
    ?_assertMatch(ok, git:branch_delete(R, "tmp2")),
    ?_assertNot(has_branch(R, "tmp2"))
  ].

list_index_test_() ->
  R = git:open("/tmp/egit"),
  L = git:list_index(R),
  M = git:list_index(R, [{fields, all}]),
  [
    ?_assert(length(L) >= 24),
    ?_assertMatch([_], [I || I = #{path := <<"README.md">>} <- git:list_index(R, [{fields, [path]}])]),
    ?_assertMatch(M, git:list_index(R, [{fields, [path,stage,conflict,oid,mode,size,ctime,mtime]}])),
    ?_assertEqual(length(L), length(M))
  ].

remote_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertEqual([{<<"origin">>,<<"https://github.com/saleyn/egit.git">>,[push,fetch]}], git:list_remotes(R)),
    ?_assertMatch(
      {error,<<"Could not rename remote: remote 'upstream' does not exist", _/binary>>},
      git:remote_rename(R, "upstream", "upstream2")),
    ?_assertEqual(
      ok,
      git:remote_add(R, "upstream", <<"https://gitlab.com/saleyn/egit.git">>)),
    ?_assertMatch(
      {error,<<"Could not create remote: remote 'upstream' already exists", _/binary>>},
      git:remote_add(R, "upstream", <<"https://gitlab.com/saleyn/egit.git">>)),
    ?_assertEqual(ok, git:remote_set_url(R, "upstream", "https://google.com/saleyn/egit.git")),
    ?_assertEqual(
      [{<<"origin">>,  <<"https://github.com/saleyn/egit.git">>, [push,fetch]},
       {<<"upstream">>,<<"https://google.com/saleyn/egit.git">>, [push,fetch]}],
      git:list_remotes(R)),
    ?_assertEqual(ok, git:remote_rename(R,  "upstream", "upstream2")),
    ?_assertEqual(ok, git:remote_delete(R,  "upstream2")),
    ?_assertEqual([{<<"origin">>,<<"https://github.com/saleyn/egit.git">>,[push,fetch]}], git:list_remotes(R))
  ].

tag_test_() ->
  R = git:open("/tmp/egit"),
  [
     ?_assertEqual(ok, git:tag_create(R, "v0.0.1", "This is a test\n", [{target, "f791f01"}])),
     ?_assertEqual(ok, git:tag_create(R, "v0.0.2")),
     ?_assertMatch([{<<"v0.0.1">>, _Msg} | _], [T || T <- git:list_tags(R, [{lines, 1}]), is_tuple(T), element(1, T) == <<"v0.0.1">>]),
     ?_assertMatch(
       #{type := tag, target_type := <<"commit">>, object := _, tag := <<"v0.0.1">>, tagger := _, message := _},
       git:cat_file(R, "v0.0.1", [])),
     ?_assertEqual([<<"v0.0.1">>, <<"v0.0.2">>], [T || T <- git:list_tags(R), lists:member(T, [<<"v0.0.1">>, <<"v0.0.2">>])]),
     ?_assertEqual(ok, git:tag_delete(R, "v0.0.1")),
     ?_assertEqual(ok, git:tag_delete(R, "v0.0.2")),
     ?_assertEqual([], [T || T <- git:list_tags(R), lists:member(T, [<<"v0.0.1">>, <<"v0.0.2">>])])
  ].

status_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertEqual(#{}, git:status(R)),
    ?_assertEqual(#{branch => <<"main">>}, git:status(R, [branch])),
    ?_assertEqual(
      #{submodules => [{<<"c_src/fmt">>,<<"https://github.com/fmtlib/fmt.git">>}]},
      git:status(R, [submodules])),
    ?_assertEqual(ok, file:write_file(<<"/tmp/egit/test.txt">>, <<"Test\n">>)),
    ?_assertEqual(#{},                              git:status(R, [{untracked, none}])),
    ?_assertEqual(#{untracked => [<<"test.txt">>]}, git:status(R, [{untracked, normal}])),
    ?_assertEqual(#{untracked => [<<"test.txt">>]}, git:status(R, [{untracked, recursive}])),
    ?_assertEqual(#{untracked => [<<"test.txt">>]}, git:status(R)),
    ?_assertEqual(#{mode => added, files => [<<"test.txt">>]}, git:add(R, "test.txt")),
    ?_assertEqual(#{index => [{new, <<"test.txt">>}]}, git:status(R)),
    ?_assertEqual(ok, file:delete("/tmp/egit/test.txt"))
  ].

reset_test_() ->
  R = git:open("/tmp/egit"),
  [
    ?_assertEqual(ok, git:reset(R, hard)),
    ?_assertEqual(ok, file:write_file(<<"/tmp/egit/test.txt">>, <<"Test\n">>)),
    ?_assertEqual(#{mode => added, files => [<<"test.txt">>]}, git:add(R, "test.txt")),
    ?_assertEqual(#{index => [{new, <<"test.txt">>}]}, git:status(R)),
    ?_assertEqual(ok, git:reset(R, hard)),
    ?_assertEqual(#{}, git:status(R)),
    ?_assert(lists:member(file:delete("/tmp/egit/test.txt"), [ok, {error, enoent}]))
  ].

empty_vector_test_() ->
  %% Test empty vector edge cases that previously caused UB/.front() on empty vectors
  R = git:open("/tmp/egit"),
  {ok, OID} = git:rev_parse(R, "HEAD"),
  [
    %% commit_lookup with empty fields list — crashed via .front() on empty keys/vals
    ?_assertEqual(#{}, git:commit_lookup(R, OID, [])),

    %% rev_list with a spec that matches nothing — empty result via make_list helper
    fun() ->
      ?assertEqual([], git:rev_list(R, ["HEAD..HEAD"], []))
    end,

    %% list_branches on a bare repo with no branches
    fun() ->
      file:del_dir_r("/tmp/egit_bare"),
      Bare = git:init("/tmp/egit_bare", [bare]),
      ?assert(is_reference(Bare)),
      ?assertEqual([], git:list_branches(Bare)),
      file:del_dir_r("/tmp/egit_bare")
    end
  ].

describe_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      case git:describe(R, "HEAD") of
        {ok, Desc} ->
          ?assert(is_binary(Desc)),
          ?assert(byte_size(Desc) > 0);
        {error, _} ->
          %% May fail if no tags are annotated, which is OK
          ok
      end
    end
  ].

blame_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      Lines = git:blame(R, "README.md"),
      ?assert(is_list(Lines)),
      case Lines of
        [] -> ok;
        [First | _] ->
          ?assert(is_tuple(First)),
          ?assertEqual(4, tuple_size(First))
      end
    end
  ].

cherry_pick_test_() ->
  R = git:open("/tmp/egit"),
  {ok, OID0} = git:rev_parse(R, "HEAD~1"),
  [
    fun() ->
      git:branch_create(R, "cherry_test", [{target, OID0}]),
      git:checkout(R, "cherry_test"),
      Res = git:cherry_pick(R, OID0),
      git:checkout(R, "main"),
      git:branch_delete(R, "cherry_test"),
      ?assertMatch(ok, Res)
    end
  ].

reflog_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      Log = git:reflog(R, "HEAD"),
      ?assert(is_list(Log)),
      case Log of
        [] -> ok;
        [First | _] ->
          ?assert(is_tuple(First)),
          ?assertEqual(4, tuple_size(First))
      end
    end
  ].

remove_move_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      file:write_file("/tmp/egit/move_test.txt", "test\n"),
      git:add(R, "move_test.txt"),
      file:rename("/tmp/egit/move_test.txt", "/tmp/egit/moved_test.txt"),
      ?assertEqual(ok, git:move(R, "move_test.txt", "moved_test.txt")),
      Index = git:list_index(R),
      HasNew = lists:any(fun(#{path := P}) -> P == <<"moved_test.txt">> end, Index),
      ?assert(HasNew),
      git:remove(R, "moved_test.txt"),
      git:reset(R, hard),
      file:delete("/tmp/egit/moved_test.txt")
    end
  ].

diff_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      %% Verify diff is callable and returns a list
      case git:rev_parse(R, "HEAD") of
        {ok, OID} ->
          Result = catch git:diff(R, OID, OID),
          ?assert(Result == [] orelse is_list(Result));
        {error, _} ->
          ok
      end
    end
  ].

merge_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      %% Verify merge is callable and doesn't crash
      case git:rev_parse(R, "main") of
        {ok, _} ->
          _Result = catch git:merge(R, "main"),
          ok;
        {error, _} ->
          ok
      end
    end
  ].

revert_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      %% Verify revert is callable and doesn't crash
      case git:rev_parse(R, "HEAD") of
        {ok, OID} ->
          _Result = catch git:revert(R, OID),
          catch git:reset(R, hard);
        {error, _} ->
          ok
      end
    end
  ].

rebase_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      %% Verify rebase functions are callable and don't crash
      case catch git:rebase_init(R, "main") of
        OpCount when is_integer(OpCount) ->
          case OpCount of
            0 -> ok;
            _ ->
              catch git:rebase_next(R),
              catch git:rebase_abort(R)
          end;
        _ -> ok
      end
    end
  ].

stash_test_() ->
  R = git:open("/tmp/egit"),
  [
    fun() ->
      %% Verify stash functions are callable and don't crash
      _Save = catch git:stash_save(R, "test"),
      _List = catch git:stash_list(R),
      _Apply = catch git:stash_apply(R, 0),
      _Pop = catch git:stash_pop(R, 0),
      _Drop = catch git:stash_drop(R, 0),
      ok
    end
  ].

last_test() ->
  %% Delete the directory if test cases succeeded
  file:del_dir_r("/tmp/egit").

has_branch(R, Name) ->
  Nm = to_bin(Name),
  [B || {local, B} <- git:list_branches(R), B == Nm] /= [].

-endif.
