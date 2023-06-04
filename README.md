# egit - Erlang interface to Git

[![build](https://github.com/saleyn/egit/actions/workflows/erlang.yml/badge.svg)](https://github.com/saleyn/egit/actions/workflows/erlang.yml)

This project is an Erlang NIF wrapper to `libgit2` library.

The project is currently in the alpha stage and more functionality is being
added.

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

## Author

Serge Aleynikov <saleyn@gmail.com>


## License

Apache 2.0
