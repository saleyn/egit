# egit - Erlang interface to Git

[![build](https://github.com/saleyn/egit/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/egit/actions/workflows/erlang.yml)

This project is an Erlang NIF wrapper to `libgit2` library. It allows to
execute commands to access and manage a `git` repository without depending
on the external `git` tool.

The project is currently in the alpha stage and more functionality is being
added.

## Currently supported functionality

- Clone a repository
- Open a repository at given local path
- Fetch from remote
- Pull from remote
- Add files to repository
- Commit
- Checkout
- Cat-file
- Rev-parse
- Rev-list
- Branch list/create/rename/delete
- Configuration get/set at various levels (e.g. system/global/local/app/default)

## Installation

- Make sure you have `libgit2` installed.
    - On Ubuntu run: `sudo apt-get install libgit2-dev`
    - On Arch Linux run: `sudo pacman -S libgit2`
    - On Mac OS run: `brew install libgit2`

- If you are building locally from source, clone [egit](https://github.com/saleyn/egit)
and run:
```shell
$ make
```

- For Erlang projects add dependency in `rebar.config`:
```erlang
{deps,
 [% ...
  {egit, "~> 0.1"}
 ]}.
```

- For Elixir projects add dependency in `mix.exs`:
```elixir
def deps do
 [
  egit: "~> 0.1"
 ]
end
```

## Usage

Include
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

The cloned/opened repository resource is owned by the current process,
and will be automatically garbage collected when the current process
exits.

After obtaining a repository reference, you can call functions in the
`egit` module. E.g.:

```erlang
2> git:branch_create(R, "tmp", [{target, <<"1b74c46">>}]).
ok
3> git:checkout(R, "tmp").
ok
4> file:write_file("/tmp/egit/temp.txt", <<"This is a test">>).
ok
5> git:add(R, ".", [verbose]).
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

## Patching

If you find some functionality lacking, feel free to add missing functions
and submit a PR.  The implementation recommendation would be to use one of
the [examples](https://github.com/libgit2/libgit2/tree/main/examples)
provided with `libgit2`, add it as `c_src/egit_*.hpp`, and modify `egit.cpp`
accordingly.

## Author

Serge Aleynikov <saleyn@gmail.com>


## License

Apache 2.0
