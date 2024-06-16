-module(rest_api_positive_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------
all() -> 
    [
    test_add,
    test_update,
    test_get_key,
    test_get_all,
    test_delete
    ].

%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Description: Initialization before the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:ensure_all_started(samurai_kv),
    application:ensure_all_started(gun),

    {ok, Port} = application:get_env(samurai_kv_http_api, http_port),
    ConfigAdd = [
                 {port, Port}, 
                 {path, "/api/keys/"}, 
                 {host, "localhost"}
                ],
    ConfigAdd ++ Config.

%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> term() | {save_config,Config1}
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Cleanup after the suite.
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    application:stop(samurai_kv),
    application:stop(gun),
    ok.

%%--------------------------------------------------------------------
%% Function: init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%%
%% TestCase = atom()
%%   Name of the test case that is about to run.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Description: Initialization before each test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%--------------------------------------------------------------------
init_per_testcase(test_add, Config) ->
    Config;
init_per_testcase(test_get_all, Config) ->
    samurai_kv:add(<<"shogun">>, <<"tokugawa">>),
    samurai_kv:add(<<"daimyo">>, <<"yasui">>),
    Config;
init_per_testcase(_TestCase, Config) ->
    samurai_kv:add(<<"shogun">>, <<"tokugawa">>),
    Config.

%%--------------------------------------------------------------------
%% Function: end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for failing the test case.
%%
%% Description: Cleanup after each test case.
%%--------------------------------------------------------------------
end_per_testcase(test_get_all, _Config) ->
    samurai_kv:delete(<<"shogun">>),
    samurai_kv:delete(<<"daimyo">>),
    ok;
end_per_testcase(_TestCase, _Config) ->
    samurai_kv:delete(<<"shogun">>),
    ok.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: test_add(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% 
%% Description: Positive test case for additon of the KV-pair to 
%% cache storage..
%%--------------------------------------------------------------------
test_add(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",
    ReqValue = "tokugawa",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:post(CPid, Path, ?header_content_type_url_encoded, ?url_encode([{"key", Key}, {"value", ReqValue}])), % post request
    {Status, Headers, Body} = test_util:receive_http_response(CPid, SRef),
    #{value := DbValue} = samurai_kv:get(list_to_binary(Key)), % get posted value from cache
    gun:cancel(CPid,SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(201, Status), % check that status code has expected values
    ?assertEqual(jiffy:encode(#{key => list_to_binary(Key), message => <<"key added">>}), Body),
    ?assert(lists:member(?content_type_json, Headers)),
    ?assertEqual(list_to_binary(ReqValue), DbValue),
    ok.

%%--------------------------------------------------------------------
%% Function: test_update(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Positive test case for update of the value for KV-pair in
%% cache storage..
%%--------------------------------------------------------------------
 test_update(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",
    ReqValue = "kamakura",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:put(CPid, Path ++ Key ,?header_content_type_url_encoded, ?url_encode([{"value", ReqValue}])), % post request
    {Status, Headers, Body} = test_util:receive_http_response(CPid, SRef),
    #{value := DbValue} = samurai_kv:get(list_to_binary(Key)), % get posted value from cache  
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(200, Status), % check that status code has expected values
    ?assertEqual(jiffy:encode(#{key => list_to_binary(Key), message => <<"key updated">>}), Body),
    ?assert(lists:member(?content_type_json, Headers)),
    ?assertEqual(list_to_binary(ReqValue), DbValue),  % check that value stored in cache is same as posted
    ok.

%%--------------------------------------------------------------------
%% Function: test_get_key(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Positive test case for extraction of KV-pair from
%% cache storage.
%%--------------------------------------------------------------------
 test_get_key(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:get(CPid, Path ++ Key, ?header_content_type_url_encoded),
    {Status, Headers, Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid,SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection

    ?assertEqual(200, Status), % check that status code has expected values
    ?assertEqual(jiffy:encode(#{key => list_to_binary(Key), value => <<"tokugawa">>}), Body), % check that response body has expected form 
    ?assert(lists:member(?content_type_json, Headers)), %  check that returned content type is json
    ok.

%%--------------------------------------------------------------------
%% Function: test_get_all(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Positive test case for extraction of all KV-pairs from
%% cache storage.
%%--------------------------------------------------------------------
 test_get_all(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:get(CPid, Path, ?header_content_type_url_encoded),
    {Status, Headers, Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid,SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection

    ?assertEqual(200, Status), % check that status code has expected values
    ?assertEqual(jiffy:encode([#{key => <<"daimyo">>, value => <<"yasui">>},
                               #{key => <<"shogun">>, value => <<"tokugawa">>}]), Body), % check that response body has expected form 
    ?assert(lists:member(?content_type_json, Headers)), %  check that returned content type is json

    ok.
%%--------------------------------------------------------------------
%% Function: test_delete(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Positive test case for removal of KV-pair from
%% cache storage.
%%--------------------------------------------------------------------
 test_delete(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    %% get request
    SRef = gun:delete(CPid, Path ++ Key, ?header_content_type_url_encoded),
    {Status, _Headers, _Body} = test_util:receive_http_response(CPid, SRef), % receive the response for DELETE request
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(204, Status), % check that status code has expected values
    ok.