# egit - Erlang interface to Git

[![build](https://github.com/saleyn/egit/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/egit/actions/workflows/erlang.yml)
[![Hex.pm](https://img.shields.io/hexpm/v/egit.svg)](https://hex.pm/packages/egit)
[![Hex.pm](https://img.shields.io/hexpm/dt/egit.svg)](https://hex.pm/packages/egit)

This project is an Erlang NIF wrapper to `libgit2` library. It allows to
execute commands to access and manage a `git` repository without depending
on the external `git` tool and internally doesn't involve any parsing of
text output produced by the `git` executable.

Though it appears to be stable, the project is currently in the beta stage.

Source code:   https://github.com/saleyn/egit

Documentation: https://hexdocs.pm/egit

## Currently supported functionality

### Repository Management
- Init a repository (including creation of bare repositories)
- Clone a repository
- Open a repository at given local path

### Remote Operations
- Fetch from remote
- Pull from remote
- Push to remote
- List/add/delete/rename/set-url on remotes

### Branch Management
- List branches (local/remote/all with filters)
- Create branches
- Rename branches
- Delete branches

### File Operations
- Add files to repository
- Remove files from index
- Move/rename tracked files
- Checkout revisions/branches
- Get repository status
- List files in index

### Commit Operations
- Commit staged changes
- Look up commit details
- Reverse parse references
- List commits with sorting/filtering

### Analysis & History
- Show file change history by line (`blame`)
- Describe working tree state (`describe`)
- Show reference logs (`reflog`)

### Configuration
- Get/set configuration at various levels (system/global/local/app/default)

### Tagging
- List/create/delete tags

### Inspection & Manipulation
- Cat-file (inspect blobs, trees, commits, tags)
- Reset (soft/mixed/hard)
- Cherry-pick commits

### Advanced Operations
- **Diff** - Compare revisions with file-level change tracking
- **Merge** - Merge branches with fast-forward and conflict detection
- **Revert** - Create commits that undo changes safely
- **Rebase** - Interactive rebase with init/next/finish/abort operations
- **Stash** - Save/list/apply/pop/drop uncommitted changes

## Supported Functions Summary

| Category | Functions |
|----------|-----------|
| Repository | init, clone, open |
| Remote | fetch, pull, push, add, delete, rename, set-url, list |
| Branch | create, delete, rename, list |
| File | add, remove, move, checkout, status, list_index |
| Commit | commit, lookup, rev-parse, rev-list, cherry-pick |
| Analysis | blame, describe, reflog, diff |
| Tag | create, delete, list |
| Configuration | get, set |
| Advanced | merge, revert, rebase, stash |
| **Total** | |

## Installation

- Make sure you have `libgit2` installed.
    - On Ubuntu run: `sudo apt-get install libgit2-dev`
    - On Arch Linux run: `sudo pacman -S libgit2`
    - On Mac OS run: `brew install libgit2`
- If you have an older gcc or clang compiler, and prefer to use a globally installed `fmt` library instead of having `egit` pull a `fmt` submodule, install it with:
    - On Mac OS run: `brew install fmt`
- If you are building locally from source, clone [egit](https://github.com/saleyn/egit)
and run:
```shell
$ make
```

- For Erlang projects add the dependency in `rebar.config`:
```erlang
{deps,
 [% ...
  {egit, "~> 0.2"}
 ]}.
```

- For Elixir projects add the dependency in `mix.exs`:
```elixir
def deps do
  [
    {:egit, "~> 0.2"}
  ]
end
```

## Usage

To clone a repository, give it a URL and a local path:
```erlang
1> Repo = git:clone("http://github.com/saleyn/egit.git", "/tmp/egit").
#Ref<...>
```

To open a local repository, give it a path:
```erlang
1> Repo = git:open(~"/tmp/egit").
#Ref<...>
```

All functions accept either charlists or binaries as arguments, so
they work conveniently in Erlang and Elixir.

The cloned/opened repository resource is owned by the current process,
and will be automatically garbage collected when the owner process
exits.

After obtaining a repository reference, you can call functions in the
`git` module as illustrated below. For complete reference of supported
functions see the [documentation](https://hexdocs.pm/egit/git.html).

### Basic Workflow

Here's a typical workflow for working with repositories:

```erlang
%% Repository initialization and opening
1> Repo = git:init("/tmp/my_repo").          % Create new repository
2> Repo = git:clone(URL, "/tmp/cloned").     % Clone from remote
3> Repo = git:open("/existing/repo").        % Open existing repository

%% Check repository status
4> git:status(Repo).
#{untracked => [<<"file.txt">>]}

%% Make changes
5> git:add(Repo, "file.txt").
#{mode => added, files => [<<"file.txt">>]}

%% Commit changes
6> git:commit(Repo, "Add new file").
{ok, <<"abc123def456...">>}

%% Push to remote
7> git:push(Repo).
ok
```

### Branch Management

```erlang
%% Create and work with branches
1> git:branch_create(Repo, "feature/new-feature").
ok

2> git:checkout(Repo, "feature/new-feature").
ok

3> git:list_branches(Repo, [local]).
[{local, <<"main">>}, {local, <<"feature/new-feature">>}]

%% Rename and delete branches
4> git:branch_rename(Repo, "feature/new-feature", "feature/better-name").
ok

5> git:branch_delete(Repo, "feature/old-branch").
ok
```

### Advanced Operations

```erlang
%% Analyze changes with diff
1> git:diff(Repo, "HEAD~1", "HEAD").
[{<<"src/module.erl">>, <<"modified">>, 2, 45}]

%% Merge branches
2> git:merge(Repo, "develop").
{ok, merged}

%% Safe undo with revert
3> git:revert(Repo, "abc123def456").
ok

%% Stash uncommitted work
4> git:stash_save(Repo, "WIP: feature work").
{ok, <<"stash_oid">>}

5> git:stash_list(Repo).
[{0, <<"WIP: feature work">>}]

6> git:stash_apply(Repo, 0).
ok

%% Rebase for clean history
7> git:rebase_init(Repo, "main").
5

8> git:rebase_finish(Repo).
ok
```

### Code Analysis

```erlang
%% Show who made changes
1> git:blame(Repo, "src/main.erl").
[{1, {<<"John Doe">>, <<"john@example.com">>}, <<"abc123">>, 1686195121},
 {2, {<<"Jane Smith">>, <<"jane@example.com">>}, <<"def456">>, 1686195200}]

%% Describe position relative to tags
2> git:describe(Repo, "HEAD").
{ok, <<"v1.0.0-5-ga8f5d2c">>}

%% View reference history
3> git:reflog(Repo, "HEAD").
[{<<"abc123">>, <<"commit: Initial commit">>, <<"John Doe">>, 1686195121}]

%% Cherry-pick commits
4> git:cherry_pick(Repo, "feature/other-branch").
ok
```

### Tag Management

```erlang
%% Create and manage tags
1> git:tag_create(Repo, "v1.0.0", "Release version 1.0.0").
ok

2> git:list_tags(Repo).
[<<"v0.9.0">>, <<"v1.0.0">>]

3> git:list_tags(Repo, [{pattern, "v1.*"}]).
[<<"v1.0.0">>]

%% Get tag details
4> git:cat_file(Repo, "v1.0.0").
#{type => tag,
  target_type => <<"commit">>,
  object => <<"abc123...">>,
  tag => <<"v1.0.0">>,
  tagger => {<<"Jane Smith">>, <<"jane@example.com">>, 1686195200},
  message => <<"Release version 1.0.0\n">>}
```

### Remote Management

```erlang
%% Configure remotes
1> git:remote_add(Repo, "upstream", "https://github.com/upstream/repo.git").
ok

2> git:list_remotes(Repo).
[{<<"origin">>, <<"https://github.com/user/repo.git">>, [push, fetch]},
 {<<"upstream">>, <<"https://github.com/upstream/repo.git">>, [push, fetch]}]

%% Sync with remote
3> git:fetch(Repo, "origin").
ok

4> git:pull(Repo, "origin").
ok

5> git:push(Repo, "origin", ["main"]).
ok
```

### Erlang Example

```erlang
2> git:branch_create(R, "tmp", [{target, ~"1b74c46"}]).
ok
3> git:checkout(R, "tmp").
ok
4> file:write_file("/tmp/egit/temp.txt", ~"This is a test").
ok
5> git:add(R, ".").
#{mode => added,files => [~"temp.txt"]}
6> git:commit(R, "Add test files").
ok
7> git:cat_file(R, ~"tmp", [{abbrev, 5}]).
#{type => commit,
  author =>
      {~"Serge Aleynikov",~"test@gmail.com",1686195121, -14400},
  oid => ~"b85d0",
  parents => [~"1fd4b"]}
8> git:cat_file(R, "b85d0", [{abbrev, 5}]).
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
9> git:tag_create(R, "v0.1.0", "Release 0.1.0").
ok
10> git:list_tags(R).
[~"v0.1.0"]
11> git:list_tags(R, [{lines, 1}]).
[{~"v0.1.0",~"Release 0.1.0\n"}]
12> git:tag_delete(R, "v0.1.0").
ok
13> git:status(R).
#{untracked => [~"temp.txt"]}
14> git:status(R, [branch]).
#{branch => ~"main", untracked => [~"temp.txt"]}
15> git:reset(R, hard).
ok
16> git:blame(R, "README.md").
[{1, {~"Serge Aleynikov", ~"test@gmail.com"}, ~"abc123", 1686195121},
 {2, {~"Jane Smith", ~"jane@example.com"}, ~"def456", 1686195200}]
17> git:describe(R, "HEAD").
{ok, ~"v0.1.0-5-ga8f5d2c"}
18> git:reflog(R, "HEAD").
[{~"abc123", ~"commit: Add feature", ~"John Doe", 1686195121},
 {~"def456", ~"checkout: moving from main to feature", ~"Jane Smith", 1686195200}]
19> git:cherry_pick(R, ~"abc123def456").
ok
20> git:remove(R, "old_file.txt").
ok
21> file:rename("/tmp/egit/old.erl", "/tmp/egit/new.erl").
ok
22> git:move(R, "old.erl", "new.erl").
ok
```

### Elixir example

```elixir
iex(1)> repo = :git.init("/tmp/egit_repo")
#Reference<0.739271388.2889220102.160795>
iex(2)> :git.remote_add(repo, "origin", "git@github.com:saleyn/test_repo.git")
:ok
iex(3)> :git.list_remotes(repo)
[{"origin", "git@github.com:saleyn/test_repo.git", [:push, :fetch]}]
iex(4)> ok = File.write!("/tmp/egit_repo/README.md", "This is a test\n")
:ok
iex(5)> :git.add(repo, "README.md")
%{mode: :added, files: ["README.md"]}
iex(6)> :git.status(repo)
%{index: [{:new, "README.md"}]}
iex(7)> :git.commit(repo, "Initial commit")
{:ok, "dc89c6b26b22f41d34300654f8d36252925d5d67"}
```

## Patching

If you find some functionality lacking, feel free to add missing functions
and submit a PR.  The implementation recommendation would be to use one of
the [examples](https://github.com/libgit2/libgit2/tree/main/examples)
provided with `libgit2` as a guide, add the functionality as `lg2_*()`
function in `c_src/git_*.hpp`, modify `git.cpp` to call that function
accordingly, write unit tests in `git.erl` and sumbmit a pull request.

## Author

Serge Aleynikov <saleyn@gmail.com>

## License

Apache 2.0
