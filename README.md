# egit - Erlang interface to Git

[![build](https://github.com/saleyn/egit/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/egit/actions/workflows/erlang.yml)

This project is an Erlang NIF wrapper to `libgit2` library. It allows to
execute commands to access and manage a `git` repository without depending
on the external `git` tool and internally doesn't involve any parsing of
text output produced by the `git` executable.

Though it appears to be stable, the project is currently in the beta stage.

Source code:   https://github.com/saleyn/egit

Documentation: https://hexdocs.pm/egit

## Currently supported functionality

- Init a repository (including creation of bare repositories)
- Clone a repository
- Open a repository at given local path
- Fetch from remote
- Pull from remote
- Push to remote
- Add files to repository
- Commit
- Checkout
- Get status
- Cat-file
- Rev-parse
- Rev-list
- Branch list/create/rename/delete
- Configuration get/set at various levels (e.g. system/global/local/app/default)
- List files in index
- List/add/delete/rename/set-url on a remote
- List/create/delete tags
- Reset

## Installation

- Make sure you have `libgit2` installed.
    - On Ubuntu run: `sudo apt-get install libgit2-dev`
    - On Arch Linux run: `sudo pacman -S libgit2`
    - On Mac OS run: `brew install libgit2`
- If you have an older gcc or clang compiler, and prefer to use a globally installed `fmt`
  library instead of having `egit` pull a `fmt` submodule, install it with:
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
  {egit, "~> 0.1"}
 ]}.
```

- For Elixir projects add the dependency in `mix.exs`:
```elixir
def deps do
  [
    {:egit, "~> 0.1"}
  ]
end
```

## Usage

To clone a repository, give it a URL and a local path:
```erlang
1> Repo = git:clone("http://github.com/saleyn/egit.git", "/tmp").
#Ref<...>
```

To open a local repository, give it a path:
```erlang
1> Repo = git:open(<<"/tmp/egit">>).
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

### Erlang Example

```erlang
2> git:branch_create(R, "tmp", [{target, <<"1b74c46">>}]).
ok
3> git:checkout(R, "tmp").
ok
4> file:write_file("/tmp/egit/temp.txt", <<"This is a test">>).
ok
5> git:add(R, ".").
#{mode => added,files => [<<"temp.txt">>]}
6> git:commit(R, "Add test files").
ok
7> git:cat_file(R, <<"tmp">>, [{abbrev, 5}]).
#{type => commit,
  author =>
      {<<"Serge Aleynikov">>,<<"test@gmail.com">>,1686195121, -14400},
  oid => <<"b85d0">>,
  parents => [<<"1fd4b">>]}
8> git:cat_file(R, "b85d0", [{abbrev, 5}]).
#{type => tree,
  commits =>
      [{<<".github">>,<<"tree">>,<<"1e41f">>,16384},
       {<<".gitignore">>,<<"blob">>,<<"b893a">>,33188},
       {<<".gitmodules">>,<<"blob">>,<<"2550a">>,33188},
       {<<".vscode">>,<<"tree">>,<<"c7b1b">>,16384},
       {<<"LICENSE">>,<<"blob">>,<<"d6456">>,33188},
       {<<"Makefile">>,<<"blob">>,<<"2d635">>,33188},
       {<<"README.md">>,<<"blob">>,<<"7b3d0">>,33188},
       {<<"c_src">>,<<"tree">>,<<"147f3">>,16384},
       {<<"rebar.config">>,<<"blob">>,<<"1f68a">>,33188},
       {<<"rebar.lock">>,<<"blob">>,<<"57afc">>,33188},
       {<<"src">>,<<"tree">>,<<"1bccb">>,16384}]}
8> git:cat_file(R, "b893a", [{abbrev, 5}]).
#{type => blob,
  blob => <<"*.swp\n*.dump\n/c_src/*.o\n/c_src/fmt\n/priv/*.so\n/_build\n/doc\n">>}
```

### Elixir example

```elixir
iex(1)> repo = :git.init("/tmp/egit_repo")
#Reference<0.739271388.2889220102.160795>
iex(2)> :git.remote_add(repo, "origin", "git@github.com:saleyn/test_repo.git")
:ok
iex(3)> :git.list_remotes(repo)
[{"origin", "git@github.com:saleyn/test_repo.git", [:push, :fetch]}]
iex(4)> ok = File.write!("/tmp/egit_repo/README.md", <<"This is a test\n">>)
:ok
iex(5)> :git.add(repo, "README.md")
%{mode: :added, files: ["README.md"]}
iex(6)> :git.status(repo)
[%{index: [{:new, "README.md"}]}]
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
