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
1> Repo = egit:clone("http://github.com/saleyn/egit.git", "/tmp").
#Ref<...>
```

To open a local repository, give it a path:
```erlang
1> Repo = egit:open(<<".">>).
#Ref<...>
```

The cloned/opened repository resource is owned by the current process,
and will be automatically garbage collected when the current process
exits.

After obtaining a repository reference, you can call functions in the
`egit` module. E.g.:

```erlang
2> egit:cat_file(Repo, <<"main">>).
{ok,{commit,#{message => <<"Initial commit\n">>,
              author =>
                  {<<"Serge Aleynikov">>,<<"saleyn@gmail.com">>,1685857770,
                   -14400},
              committer =>
                  {<<"Serge Aleynikov">>,<<"saleyn@gmail.com">>,1685857770,
                   -14400},
              parents => []}}}
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
