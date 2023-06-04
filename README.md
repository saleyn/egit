# egit - Erlang interface to Git

This project is an Erlang NIF wrapper to `libgit2` library

## Installation

Make sure you have `libgit2` installed.

- On Ubuntu run: `sudo apt-get install libgit2-dev`
- On Arch Linux run: `sudo pacman -S libgit2`
- On Mac OS run: `brew install libgit2`

After that run:
```shell
$ make
```

## Usage

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
2> egit:cat_file(Repo, <<"master">>).
```

## Author

Serge Aleynikov <saleyn at gmail dot com>


## License

Apache 2.0
