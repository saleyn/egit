-module(egit).
-export([clone/2, open/1, fetch/1, fetch/2, pull/1, pull/2, commit_lookup/3]).
-export([cat_file/2, cat_file/3]).

-on_load(init/0).

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

-type cat_file_opt() :: type | size | all.

-export_type([repository/0, commit_opt/0]).

-define(LIBNAME, ?MODULE).
-define(NOT_LOADED_ERROR,
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, ?LINE}]})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
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

%% @doc Clone a remote repository to the local path
-spec clone(binary(), binary()) -> repository().
clone(URL, Path) when is_binary(URL), is_binary(Path) ->
  ?NOT_LOADED_ERROR.

%% @doc Open a local git repository
-spec open(binary()) -> repository().
open(Path) when is_binary(Path) ->
  ?NOT_LOADED_ERROR.

%% @doc Fetch from origin
-spec fetch(repository()) -> ok | {error, binary()}.
fetch(Repo)         -> fetch_or_pull(Repo, fetch).

%% @doc Fetch from given remote (e.g. <<"origin">>)
-spec fetch(repository(), binary()) -> ok | {error, binary()}.
fetch(Repo, Remote) -> fetch_or_pull(Repo, fetch, Remote).

%% @doc Pull from origin
-spec pull(repository()) -> ok | {error, binary()}.
pull(Repo)          -> fetch_or_pull(Repo, pull).

%% @doc Pull from given remote (e.g. <<"origin">>)
-spec pull(repository(), binary()) -> ok | {error, binary()}.
pull(Repo, Remote)  -> fetch_or_pull(Repo, pull, Remote).

fetch_or_pull(Repo, _Op) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

fetch_or_pull(Repo, _Op, Remote) when is_reference(Repo), is_binary(Remote) ->
  ?NOT_LOADED_ERROR.

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary()) -> {ok, term()} | {error, term()}.
cat_file(Repo, File) ->
  cat_file(Repo, File, all).

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary(), cat_file_opt()) -> {ok, term()} | {error, term()}.
cat_file(Repo, File, Opt) when is_reference(Repo), is_binary(File), is_atom(Opt) ->
  ?NOT_LOADED_ERROR.

-spec commit_lookup(repository(), binary(), [commit_opt()]) -> #{commit_opt() => term()}.
commit_lookup(_Repo, OID, Opts) when is_binary(OID), is_list(Opts) ->
  ?NOT_LOADED_ERROR.
