-module(egit).
-export([clone/2, open/1, fetch/1, fetch/2, pull/1, pull/2, commit_lookup/3]).
-export([cat_file/2, cat_file/3, checkout/2, checkout/3]).
-export([add_all/1, add/2, add/3, commit/2, rev_parse/2]).

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
-type commit_opts()     :: [commit_opt()].
-type cat_file_opt()    :: type | size | all.
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

-export_type([repository/0, commit_opts/0, cat_file_opt/0, checkout_opts/0, checkout_stats/0]).

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

%% @doc Fetch from given remote (e.g. `<<"origin">>')
-spec fetch(repository(), binary()) -> ok | {error, binary()}.
fetch(Repo, Remote) -> fetch_or_pull(Repo, fetch, Remote).

%% @doc Pull from origin
-spec pull(repository()) -> ok | {error, binary()}.
pull(Repo)          -> fetch_or_pull(Repo, pull).

%% @doc Pull from given remote (e.g. `<<"origin">>')
-spec pull(repository(), binary()) -> ok | {error, binary()}.
pull(Repo, Remote)  -> fetch_or_pull(Repo, pull, Remote).

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary()) -> {ok, term()} | {error, term()}.
cat_file(Repo, Revision) ->
  cat_file(Repo, Revision, all).

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary(), cat_file_opt()) -> {ok, term()} | {error, term()}.
cat_file(Repo, Revision, Opt) when is_reference(Repo), is_binary(Revision), is_atom(Opt) ->
  ?NOT_LOADED_ERROR.

%% @doc Same as `checkout(Repo, Revision, [])'.
-spec checkout(repository(), binary()) -> ok | {error, term()}.
checkout(Repo, Revision) ->
  checkout(Repo, Revision, []).

%% @doc Provide content or type and size information for repository objects.
%% If `Opts' contains `verbose' (and optionally `perf'), then the return is a
%% map with checkout stats.
-spec checkout(repository(), binary(), checkout_opts()) -> ok | checkout_stats() | {error, term()}.
checkout(Repo, Revision, Opts) when is_reference(Repo), is_binary(Revision), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

%% @doc Add all pending changes
add_all(Repo) when is_reference(Repo) ->
  add_nif(Repo, [<<".">>], []).

%% @doc Same as `add(Repo, FileSpecs, Opts)'.
-spec add(repository(), binary()|[binary()]) -> [binary()] | {error, term()}.
add(Repo, PathSpecs) ->
  add(Repo, PathSpecs, []).

%% @doc Add files matching `PathSpecs' to index.
-spec add(repository(), [binary()], add_opts()) -> [binary()] | {error, term()}.
add(Repo, PathSpec, Opts) when is_binary(PathSpec) ->
  add(Repo, [PathSpec], Opts);

add(Repo, PathSpecs, Opts) when is_reference(Repo), is_list(PathSpecs), is_list(Opts) ->
  add_nif(Repo, PathSpecs, Opts).

%% @doc Commit changes to a repository
-spec commit(repository(), binary()) -> {ok, OID::binary()} | {error, binary()|atom()}.
commit(_Repo, Comment) when is_binary(Comment) ->
  ?NOT_LOADED_ERROR.

%% @doc Reverse parse a reference
-spec rev_parse(repository(), binary()) -> map() | {error, binary()|atom()}.
rev_parse(_Repo, Spec) when is_binary(Spec) ->
  ?NOT_LOADED_ERROR.

%% @doc Lookup commit details identified by OID
-spec commit_lookup(repository(), binary(), [commit_opt()]) -> #{commit_opt() => term()}.
commit_lookup(_Repo, OID, Opts) when is_binary(OID), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
fetch_or_pull(Repo, _Op) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

fetch_or_pull(Repo, _Op, Remote) when is_reference(Repo), is_binary(Remote) ->
  ?NOT_LOADED_ERROR.

add_nif(Repo, PathSpecs, Opts) when is_reference(Repo), is_list(PathSpecs), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

-ifdef(EUNIT).

clone_test_() ->
  {setup,
    fun()  -> file:del_dir_r("/tmp/egit"), ok end,
    fun(_) -> file:del_dir_r("/tmp/egit"), ok end,
    [
      fun() ->
        R = egit:clone(<<"https://github.com/saleyn/egit.git">>, <<"/tmp/egit">>),
        ?assert(is_reference(R)),
        ?assertEqual(ok, egit:fetch(R)),
        ?assertEqual(ok, egit:pull(R)),
        ?assertEqual(ok, egit:checkout(R, <<"main">>)),
        ?assertEqual([], os:cmd("echo \"\n\" >> /tmp/egit/README.md")),
        ?assertEqual(
          #{mode => dry_run, files => [<<"README.md">>]},
          egit:add(R, <<".">>, [verbose, dry_run])),
        ?assertEqual(
          #{mode => added, files => [<<"README.md">>]},
          egit:add(R, <<".">>, [verbose])),
        ?assertEqual(
          #{mode => none,files => []},
          egit:add(R, <<".">>, [verbose])),
        {ok, OID} = egit:commit(R, <<"Test commit">>),
        ?assertEqual({ok, nil}, egit:commit(R, <<"Test commit">>)),
        ?assertMatch(#{oid := OID}, egit:rev_parse(R, <<"HEAD">>))
      end
    ]}.

-endif.