-module(egit).
-export([clone/2, open/1, fetch/1, fetch/2, pull/1, pull/2, commit_lookup/3]).
-export([cat_file/2, cat_file/3, checkout/2, checkout/3]).
-export([add_all/1, add/2, add/3, commit/2, rev_parse/2, rev_parse/3, rev_list/3]).

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

-type rev_list_opt()    :: [topo_order | date_order | reverse | {limit, pos_integer()} | {abbrev, pos_integer()}].
-type rev_list_opts()   :: [rev_list_opt()].

-type rev_parse_opt()   :: {abbrev, pos_integer()}.
-type rev_parse_opts()  :: [rev_parse_opt()].

-export_type([repository/0, commit_opts/0, cat_file_opt/0, checkout_opts/0, checkout_stats/0]).
-export_type([rev_parse_opts/0, rev_list_opts/0]).

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
-spec clone(binary()|string(), binary()|string()) -> repository().
clone(URL, Path)    -> clone_nif(to_bin(URL), to_bin(Path)).

%% @doc Open a local git repository
-spec open(binary()|string()) -> repository().
open(Path)          -> open_nif(to_bin(Path)).

%% @doc Fetch from origin
-spec fetch(repository()) -> ok | {error, binary()}.
fetch(Repo)         -> fetch_nif(Repo, fetch).

%% @doc Fetch from given remote (e.g. `<<"origin">>')
-spec fetch(repository(), binary()|string()) -> ok | {error, binary()}.
fetch(Repo, Remote) -> fetch_nif(Repo, fetch, to_bin(Remote)).

%% @doc Pull from origin
-spec pull(repository()) -> ok | {error, binary()}.
pull(Repo)          -> fetch_nif(Repo, pull).

%% @doc Pull from given remote (e.g. `<<"origin">>')
-spec pull(repository(), binary()|string()) -> ok | {error, binary()}.
pull(Repo, Remote)  -> fetch_nif(Repo, pull, to_bin(Remote)).

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary()|string()) -> {ok, term()} | {error, term()}.
cat_file(Repo, Rev) -> cat_file_nif(Repo, to_bin(Rev), all).

%% @doc Provide content or type and size information for repository objects
-spec cat_file(repository(), binary()|string(), cat_file_opt()) -> {ok, term()} | {error, term()}.
cat_file(Repo, Rev, Opt) -> cat_file_nif(Repo, to_bin(Rev), Opt).

%% @doc Same as `checkout(Repo, Revision, [])'.
-spec checkout(repository(), binary()|string()) -> ok | {error, term()}.
checkout(Repo, Rev) -> checkout_nif(Repo, to_bin(Rev), []).

%% @doc Provide content or type and size information for repository objects.
%% If `Opts' contains `verbose' (and optionally `perf'), then the return is a
%% map with checkout stats.
-spec checkout(repository(), binary(), checkout_opts()) -> ok | checkout_stats() | {error, term()}.
checkout(Repo, Revision, Opts) ->
  checkout_nif(Repo, to_bin(Revision), Opts).

%% @doc Add all pending changes
add_all(Repo) when is_reference(Repo) ->
  add_nif(Repo, [<<".">>], []).

%% @doc Same as `add(Repo, FileSpecs, Opts)'.
-spec add(repository(), binary()|string()|[binary()|string()]) -> [binary()] | {error, term()}.
add(Repo, [C|_] = PathSpec) when is_integer(C), C >= 32, C < 256 ->
  add_nif(Repo, [to_bin(PathSpec)], []);
add(Repo, PathSpec) when is_binary(PathSpec) ->
  add_nif(Repo, [PathSpec], []).

%% @doc Add files matching `PathSpecs' to index.
-spec add(repository(), [binary()|string()], add_opts()) -> [binary()] | {error, term()}.
add(Repo, [C|_] = PathSpecs, Opts) when is_integer(C), C >= 32, C < 256 ->
  add_nif(Repo, [to_bin(PathSpecs)], Opts);
add(Repo, PathSpec, Opts) when is_binary(PathSpec)->
  add_nif(Repo, [PathSpec], Opts);
add(Repo, PathSpecs, Opts) when is_list(PathSpecs)->
  add_nif(Repo, [to_bin(B) || B <- PathSpecs], Opts).

%% @doc Commit changes to a repository
-spec commit(repository(), binary()|string()) -> {ok, OID::binary()} | {error, binary()|atom()}.
commit(_Repo, Comment) ->
  commit_nif(_Repo, to_bin(Comment)).

%% @doc Reverse parse a reference.
%% See [https://git-scm.com/docs/git-rev-parse.html#_specifying_revisions]
%% for the formats of a `Spec'.
%%
%% Opts is a list of:
%% <dl>
%% <dt>{abbrev, `NumChars'}</dt>
%%   <dd>NumChars truncates the commit hash (must be <= 40)</dd>
%% </dl>
%%
%% When a reference refers to a single object, an ok tuple with a binary
%% string of the commit hash is returned.  When it refers to a range
%% (e.g. `HEAD..HEAD~2`), a map is returned with `from' and `to' keys.
%% When using a Symmetric Difference Notation `...' (i.e. `HEAD...HEAD~4'),
%% a map with three keys `from', `to', and `merge_base' is returned.
%%
%% Examples:
%% ```
%% 2> egit:rev_parse(R,<<"HEAD~4">>, [{abbrev, 7}]).
%% {ok,<<"6d6f662">>}
%% 3> egit:rev_parse(R,<<"HEAD..HEAD~4">>, [{abbrev, 7}]).
%% egit:rev_parse(R,<<"HEAD..HEAD~4">>, [{abbrev, 7}]).
%% #{from => <<"f791f01">>,to => <<"6d6f662">>}
%% 4> egit:rev_parse(R,<<"HEAD...HEAD~4">>).
%% egit:rev_parse(R,<<"HEAD...HEAD~4">>, [{abbrev, 7}]).
%% #{from => <<"f791f01">>,merge_base => <<"6d6f662">>, to => <<"6d6f662">>}
%% '''
-spec rev_parse(repository(), binary()|string(), rev_parse_opts()) -> {ok, binary()} | map() | {error, binary()|atom()}.
rev_parse(Repo, Spec, Opts) ->
  rev_parse_nif(Repo, to_bin(Spec), Opts).

%% @doc Same as `rev_parse(Repo, Spec, [])'.
-spec rev_parse(repository(), binary()|string()) -> {ok, binary()} | map() | {error, binary()|atom()}.
rev_parse(Repo, Spec) ->
  rev_parse(Repo, Spec, []).

%% @doc Return the list of OIDs for the given specs.
%%
%% Opts is a list of:
%% <dl>
%% <dt>topo_order | date_order | reverse</dt>
%%   <dd>Control sorting order</dd>
%% <dt>{limit, `Limit'}</dt>
%%   <dd>Limit is an integer that limits the number of refs returned</dd>
%% <dt>{abbrev, `NumChars'}</dt>
%%   <dd>NumChars truncates the commit hash (must be <= 40)</dd>
%% </dl>
%%
%% Example:
%% ```
%% 9> egit:rev_list(R, ["HEAD"], [{limit, 4}, {abbrev, 7}]).
%% [<<"f791f01">>,<<"1b74c46">>,<<"c40374d">>,<<"12968bd">>]
%% '''
-spec rev_list(repository(), ['not'|'Elixir.Not'|binary()], rev_list_opts()) -> #{commit_opt() => term()}.
rev_list(Repo, Specs, Opts) when is_list(Specs) ->
  F = fun
    ('not')               -> 'not';
    ('Elixir.Not')        -> 'not';
    (I) when is_list(I)   -> list_to_binary(I);
    (I) when is_binary(I) -> I
  end,
  L = [F(I) || I <-
        case Specs of
          _ when is_binary(Specs) -> [Specs];
          [C|_] when is_integer(C), C >= 40, C < 256 -> [list_to_binary(Specs)];
          _ when is_list(Specs)   -> Specs
        end],
  rev_list_nif(Repo, L, Opts).

%% @doc Lookup commit details identified by OID
-spec commit_lookup(repository(), binary()|string(), [commit_opt()]) -> #{commit_opt() => term()}.
commit_lookup(Repo, OID, Opts) ->
  commit_lookup_nif(Repo, to_bin(OID), Opts).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

to_bin(B) when is_binary(B) -> B;
to_bin(B) when is_list(B)   -> list_to_binary(B).

clone_nif(URL, Path) when is_binary(URL), is_binary(Path) ->
  ?NOT_LOADED_ERROR.

open_nif(Path) when is_binary(Path) ->
  ?NOT_LOADED_ERROR.

fetch_nif(Repo, _Op) when is_reference(Repo) ->
  ?NOT_LOADED_ERROR.

fetch_nif(Repo, _Op, Remote) when is_reference(Repo), is_binary(Remote) ->
  ?NOT_LOADED_ERROR.

add_nif(Repo, PathSpecs, Opts) when is_reference(Repo), is_list(PathSpecs), is_list(Opts) ->
  ?NOT_LOADED_ERROR.

cat_file_nif(Repo, Rev, Opt) when is_reference(Repo), is_binary(Rev), is_atom(Opt) ->
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

-ifdef(EUNIT).

clone_test_() ->
  file:del_dir_r("/tmp/egit"),
  R = egit:clone(<<"https://github.com/saleyn/egit.git">>, <<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertMatch({ok, _}, egit:rev_parse(R, <<"HEAD">>))
  ].

fetch_test_() ->
  R = egit:open(<<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, egit:fetch(R))
  ].

pull_test_() ->
  R = egit:open(<<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, egit:fetch(R))
  ].

checkout_test_() ->
  R = egit:open(<<"/tmp/egit">>),
  [
    ?_assert(is_reference(R)),
    ?_assertEqual(ok, egit:checkout(R, <<"main">>))
  ].

commit_test_() ->
  R = egit:open(<<"/tmp/egit">>),
  {ok, OID0} = egit:rev_parse(R, <<"HEAD">>),
  {setup,
    fun()  -> ok end,
    fun(_) ->
      %% Only delete the directory if test cases succeeded
      persistent_term:get(egit, undefined) == ok andalso file:del_dir_r("/tmp/egit"),
      persistent_term:erase(egit)
    end,
    [
      fun() ->
        ?assert(is_reference(R)),
        ?assertEqual([], os:cmd("echo \"\n\" >> /tmp/egit/README.md")),
        ?assertEqual(
          #{mode => dry_run, files => [<<"README.md">>]},
          egit:add(R, ".", [verbose, dry_run])),
        ?assertEqual(
          #{mode => added, files => [<<"README.md">>]},
          egit:add(R, <<".">>, [verbose])),
        ?assertEqual(
          #{mode => none,files => []},
          egit:add(R, ["."], [verbose])),
        {ok, OID0} = egit:rev_parse(R, "HEAD"),
        Res        = egit:commit(R, "Test commit"),
        ?assertMatch({ok, _}, Res),
        {ok, OID}  = Res,
        ?assertEqual({ok, nil}, egit:commit(R, "Test commit")),
        ?assertEqual({ok, OID}, egit:rev_parse(R, "HEAD"))
      end,
      fun() ->
        Res = egit:rev_parse(R, <<"HEAD">>),
        ?assertMatch({ok, OID} when is_binary(OID), Res),
        {ok, OID} = Res,
        ?assertEqual(#{to => OID0, from => OID}, egit:rev_parse(R, <<"HEAD..HEAD~1">>)),
        ?assertEqual(#{to => OID0, from => OID, merge_base => OID0}, egit:rev_parse(R, <<"HEAD...HEAD~1">>)),
        ?assertMatch({error, _}, egit:rev_parse(R, <<"HEAD~x">>))
      end,
      fun() ->
        case egit:rev_list(R, ["HEAD"], [{limit, 3}, {abbrev, 7}]) of
          [A,B,C] when is_binary(A), is_binary(B), is_binary(C),
                       byte_size(A)==7, byte_size(B)==7, byte_size(C)==7 ->
            ok;
          _Res ->
            ?assert(false)
        end
      end,
      fun() -> persistent_term:put(egit, ok) end
    ]}.

-endif.