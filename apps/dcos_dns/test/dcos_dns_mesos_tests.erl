-module(dcos_dns_mesos_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("erldns/include/erldns.hrl").
-include("dcos_dns.hrl").

all_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"None on Host", fun none_on_host/0},
        {"None on User", fun none_on_dcos/0},
        {"UCR on Host", fun ucr_on_host/0},
        {"UCR on Bridge", fun ucr_on_bridge/0},
        {"UCR on User", fun ucr_on_dcos/0},
        {"Docker on Host", fun docker_on_host/0},
        {"Docker on Bridge", fun docker_on_bridge/0},
        {"Docker on User", fun docker_on_dcos/0}
    ]}.

-define(DNAME(AppName, Type),
    <<AppName, ".marathon.", Type, ".dcos.thisdcos.directory">>).

none_on_host() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("none-on-host", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("none-on-host", "autoip"))),
    ?assertMatch(
        [],
        resolve(?DNAME("none-on-host", "containerip"))).

none_on_dcos() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("none-on-dcos", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 2, 5}}}],
        resolve(?DNAME("none-on-dcos", "autoip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 2, 5}}}],
        resolve(?DNAME("none-on-dcos", "containerip"))).

ucr_on_host() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 3}}}],
        resolve(?DNAME("ucr-on-host", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 3}}}],
        resolve(?DNAME("ucr-on-host", "autoip"))),
    ?assertMatch(
        [],
        resolve(?DNAME("ucr-on-host", "containerip"))).

ucr_on_bridge() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 3}}}],
        resolve(?DNAME("ucr-on-bridge", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 3}}}],
        resolve(?DNAME("ucr-on-bridge", "autoip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 31, 254, 3}}}],
        resolve(?DNAME("ucr-on-bridge", "containerip"))).

ucr_on_dcos() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 3}}}],
        resolve(?DNAME("ucr-on-dcos", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 1, 6}}}],
        resolve(?DNAME("ucr-on-dcos", "autoip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 1, 6}}}],
        resolve(?DNAME("ucr-on-dcos", "containerip"))).

docker_on_host() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("docker-on-host", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("docker-on-host", "autoip"))),
    ?assertMatch(
        [],
        resolve(?DNAME("docker-on-host", "containerip"))).

docker_on_bridge() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("docker-on-bridge", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("docker-on-bridge", "autoip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 18, 0, 2}}}],
        resolve(?DNAME("docker-on-bridge", "containerip"))).

docker_on_dcos() ->
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {172, 17, 0, 4}}}],
        resolve(?DNAME("docker-on-dcos", "agentip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 2, 130}}}],
        resolve(?DNAME("docker-on-dcos", "autoip"))),
    ?assertMatch(
        [#dns_rr{data=#dns_rrdata_a{ip = {9, 0, 2, 130}}}],
        resolve(?DNAME("docker-on-dcos", "containerip"))).

-define(LOCALHOST, {127, 0, 0, 1}).
resolve(DName) ->
    DNSQueries = [#dns_query{name=DName, type=?DNS_TYPE_A}],
    DNSMessage = #dns_message{
        rd=true, qc=length(DNSQueries),
        questions=DNSQueries
    },
    #dns_message{answers=DNSAnswers} =
        erldns_handler:do_handle(DNSMessage, ?LOCALHOST),
    DNSAnswers.

%%%===================================================================
%%% Setup & cleanup
%%%===================================================================

setup() ->
    meck:new(lashup_kv),
    meck:expect(lashup_kv, value, fun value/1),
    meck:expect(lashup_kv, request_op, fun request_op/2),

    {ok, Apps} = ensure_all_started(erldns),
    Tasks = dcos_net_mesos_state_tests:setup(),
    {ok, Pid} = dcos_dns_mesos:start_link(),
    true =
        lists:any(fun (_) ->
            timer:sleep(100),
            recon:get_state(dcos_dns_mesos) =/= []
        end, lists:seq(1, 20)),

    {Tasks, Pid, Apps}.

cleanup({Tasks, Pid, Apps}) ->
    unlink(Pid),
    exit(Pid, kill),
    meck:unload(lashup_kv),

    lists:foreach(fun application:stop/1, Apps),
    lists:foreach(fun application:unload/1, Apps),

    dcos_net_mesos_state_tests:cleanup(Tasks).

ensure_all_started(erldns) ->
    ok = application:load(lager),
    ok = application:load(erldns),

    {ok, Cwd} = file:get_cwd(),
    SysConfigFile = filename:join(Cwd, "config/ct.sys.config"),
    {ok, [SysConfig]} = file:consult(SysConfigFile),
    lists:foreach(fun ({App, Config}) ->
        lists:foreach(fun ({K, V}) ->
            application:set_env(App, K, V)
        end, Config)
    end, SysConfig),

    application:ensure_all_started(erldns).

%%%===================================================================
%%% Lashup mocks
%%%===================================================================

value(?LASHUP_KEY(ZoneName)) ->
    case erldns_zone_cache:get_zone_with_records(ZoneName) of
        {ok, #zone{records = Records}} ->
            [{?RECORDS_FIELD, Records}];
        {error, zone_not_found} ->
            [{?RECORDS_FIELD, []}]
    end.

request_op(LKey = ?LASHUP_KEY(ZoneName), {update, Updates}) ->
    [{?RECORDS_FIELD, Records}] = lashup_kv:value(LKey),
    Records0 = apply_op(Records, Updates),
    Sha = crypto:hash(sha, term_to_binary(Records0)),
    erldns_zone_cache:put_zone({ZoneName, Sha, Records0}),
    {ok, value(LKey)}.

apply_op(List, Updates) ->
    lists:foldl(
        fun ({update, ?RECORDS_FIELD, {remove_all, RList}}, Acc) ->
                Acc -- RList;
            ({update, ?RECORDS_FIELD, {add_all, AList}}, Acc) ->
                Acc ++ AList
        end, List, Updates).