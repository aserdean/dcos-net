%%%-------------------------------------------------------------------
%% @doc navstar public API
%% @end
%%%-------------------------------------------------------------------

-module(dcos_net_app).

-behaviour(application).

-define(MASTERS_KEY, {masters, riak_dt_orswot}).

%% Application callbacks
-export([
    start/2,
    stop/1,
    load_config_files/1,
    dist_port/0
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    load_config_files(),
    load_plugins(),
    maybe_add_master(),
    dcos_net_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

maybe_add_master() ->
    case application:get_env(dcos_net, is_master, false) of
        false ->
            ok;
        true ->
            add_master()
    end.

add_master() ->
    Masters = lashup_kv:value([masters]),
    case orddict:find(?MASTERS_KEY, Masters) of
        error ->
            add_master2();
        {ok, Value} ->
            add_master2(Value)
    end.
add_master2(OldMasterList) ->
    case lists:member(node(), OldMasterList) of
        true ->
            ok;
        false ->
            add_master2()
    end.
add_master2() ->
    lashup_kv:request_op([masters],
        {update, [
            {update, ?MASTERS_KEY, {add, node()}}
        ]}).

load_config_files() ->
    load_config_files(undefined).

-spec load_config_files(App :: atom()) -> ok.
load_config_files(App) ->
    ConfigDir = config_dir(),
    case file:list_dir(ConfigDir) of
      {ok, []} ->
        lager:info("Found an empty config directory: ~p", [ConfigDir]);
      {error, enoent} ->
        lager:info("Couldn't find config directory: ~p", [ConfigDir]);
      {ok, Filenames} ->
        lists:foreach(fun (Filename) ->
            AbsFilename = filename:absname(Filename, ConfigDir),
            load_config_file(App, AbsFilename)
        end, Filenames)
    end.

load_config_file(App, Filename) ->
    case file:consult(Filename) of
        {ok, []} ->
            lager:info("Found an empty config file: ~p~n", [Filename]);
        {error, eacces} ->
            lager:info("Couldn't load config: ~p", [Filename]);
        {ok, Result} ->
            load_config(App, Result),
            lager:info("Loaded config: ~p", [Filename])
    end.

load_config(App, [Result]) ->
    lists:foreach(fun (AppOptions) ->
        load_app_config(App, AppOptions)
    end, Result).

load_app_config(undefined, {App, Options}) ->
    load_app_config(App, {App, Options});
load_app_config(App, {App, Options}) ->
    lists:foreach(fun ({OptionKey, OptionValue}) ->
        application:set_env(App, OptionKey, OptionValue)
    end, Options);
load_app_config(_App, _AppOptions) ->
    ok.

%%====================================================================
%% dist_port
%%====================================================================

-spec(dist_port() -> {ok, inet:port_number()} | {error, atom()}).
dist_port() ->
    ConfigDir = config_dir(),
    try
        case erl_prim_loader:list_dir(ConfigDir) of
            {ok, Filenames} ->
                dist_port(Filenames, ConfigDir);
            error ->
                {error, list_dir}
        end
    catch _:Err ->
        {error, Err}
    end.

-spec(dist_port([file:filename()], file:filename()) ->
    {ok, inet:port_number()} | {error, atom()}).
dist_port([], _Dir) ->
    {error, not_found};
dist_port([Filename|Filenames], Dir) ->
    AbsFilename = filename:absname(Filename, Dir),
    case consult(AbsFilename) of
        {ok, Data} ->
            case find(dcos_net, dist_port, Data) of
                {ok, Port} ->
                    {ok, Port};
                {error, _Error} ->
                    dist_port(Filenames, Dir)
            end;
        {error, _Error} ->
            dist_port(Filenames, Dir)
    end.

-spec(consult(file:filename()) -> {ok, term()} | {error, term()}).
consult(Filename) ->
    case prim_file:read_file(Filename) of
        {ok, Data} ->
            String = binary_to_list(Data),
            case erl_scan:string(String) of
                {ok, Tokens, _Line} ->
                    erl_parse:parse_term(Tokens);
                {error, Error, _Line} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec(find(atom(), atom(), [{atom(), [{atom(), term()}]}]) ->
    {ok, inet:port_number()} | {error, atom()}).
find(App, Key, Data) ->
    case lists:keyfind(App, 1, Data) of
        {App, Config} ->
            case lists:keyfind(Key, 1, Config) of
                {Key, Port} ->
                    {ok, Port};
                false ->
                    {error, not_found}
            end;
        false ->
            {error, not_found}
    end.


%%====================================================================
%% config dir
%%====================================================================

-define(DEFAULT_CONFIG_DIR, "/opt/mesosphere/etc/dcos-net.config.d").

-type config_dir_r() :: {ok, file:filename()} | undefined.

-spec(config_dir() -> file:filename()).
config_dir() ->
    config_dir([
        fun config_dir_env/0,
        fun config_dir_arg/0,
        fun config_dir_sys/0
    ]).

-spec(config_dir([fun (() -> config_dir_r())]) -> file:filename()).
config_dir([]) ->
    ?DEFAULT_CONFIG_DIR;
config_dir([Fun|Funs]) ->
    case Fun() of
        {ok, ConfigDir} ->
            ConfigDir;
        undefined ->
            config_dir(Funs)
    end.

-spec(config_dir_env() -> config_dir_r()).
config_dir_env() ->
    application:get_env(dcos_net, config_dir).

-spec(config_dir_arg() -> config_dir_r()).
config_dir_arg() ->
    case init:get_argument(dcos_net) of
        {ok, Args} ->
            case [V || ["config_dir", V] <- Args] of
                [V|_] -> {ok, V};
                [] -> undefined
            end;
        error -> undefined
    end.

-spec(config_dir_sys() -> config_dir_r()).
config_dir_sys() ->
    case init:get_argument(config) of
        {ok, [SysConfig|_]} ->
            config_dir_sys(SysConfig);
        error -> undefined
    end.

-spec(config_dir_sys(file:filename()) -> config_dir_r()).
config_dir_sys(SysConfig) ->
    case consult(SysConfig) of
        {ok, Data} ->
            case find(dcos_net, config_dir, Data) of
                {ok, ConfigDir} ->
                    {ok, ConfigDir};
                {error, _Error} ->
                    undefined
            end;
        {error, _Error} ->
            undefined
    end.

%%====================================================================
%% Plugins
%%====================================================================

load_plugins() ->
    Plugins = application:get_env(dcos_net, plugins, []),
    lists:foreach(fun load_plugin/1, Plugins).

load_plugin({App, AppPath}) ->
    case code:add_pathz(AppPath) of
        true ->
            load_modules(App, AppPath),
            case application:ensure_all_started(App, permanent) of
                {error, Error} ->
                    lager:error("Plugin ~p: ~p", [App, Error]);
                {ok, _Apps} -> ok
            end;
        {error, bad_directory} ->
            lager:error("Plugin ~p: bad_directory", [App])
    end.

load_modules(App, AppPath) ->
    AppFile = filename:join(AppPath, [App, ".app"]),
    {ok, [{application, App, AppData}]} = file:consult(AppFile),
    {modules, Modules} = lists:keyfind(modules, 1, AppData),
    lists:foreach(fun code:load_file/1, Modules).
