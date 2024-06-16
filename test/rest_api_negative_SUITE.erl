-module(rest_api_negative_SUITE).
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
    test_add_empty_body,
    test_add_already_exists,
    test_add_value_toolong,
    test_update_key_provided_not_exists,
    test_update_empty_body,
    test_update_wrong_path,
    test_get_key_provided_not_exists,
    test_delete_key_not_provided,
    test_delete_key_provided_not_exists
    ].

%--------------------------------------------------------------------
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
init_per_testcase(TestCase, Config) when TestCase =:= test_add_already_exists;
                                         TestCase =:= test_update_empty_body ->
    samurai_kv:add(<<"shogun">>, <<"tokugawa">>),
    Config;
init_per_testcase(test_add_storage_full, Config) ->
    samurai_kv:add(<<"shogun">>, <<"tokugawa">>),
    samurai_kv:add(<<"daimyo">>, <<"yasui">>),
    Config;
init_per_testcase(_TestCase, Config) ->
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
end_per_testcase(test_add_storage_full, _Config) ->
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
%% Function: test_add_empty_body(Config0) -> ok
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for trying to send empty body in post
%% request.
%%--------------------------------------------------------------------
test_add_empty_body(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:post(CPid, Path, ?header_content_type_url_encoded, ""), 
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(400, Status), % check that status code has expected values
    ok.

%%--------------------------------------------------------------------
%% Function: test_add_already_exists(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for insertion/update of the KV-pair in 
%% cache storage engine.
%%--------------------------------------------------------------------
test_add_already_exists(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",
    Value = "tokugawa",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:post(CPid, Path, ?header_content_type_url_encoded, ?url_encode([{"key", Key}, {"value", Value}])), % post request
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(304, Status), % check that status code has expected values
    ?assertEqual([], Body),
    ok.

%%--------------------------------------------------------------------
%% Function: test_add_storage_full(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for insertion/update of the KV-pair in 
%% cache storage engine.
%%--------------------------------------------------------------------
test_add_storage_full(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "rurounin",
    Value = "kenshin",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:post(CPid, Path, ?header_content_type_url_encoded, ?url_encode([{"key", Key}, {"value", Value}])), % post request
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(503, Status), % check that status code has expected values
    ?assertEqual(<<"{\"error\":\"storage size limit exceeded\"}">>, Body),
    ok.

%%--------------------------------------------------------------------
%% Function: test_add_value_toolong(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%%
%%
%% Description: Negative test case for insertion/update of the KV-pair in 
%% cache storage engine.
%%--------------------------------------------------------------------
test_add_value_toolong(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config), % get the pre-defined server tcp port
    Key = "shogun",
    Value = "tokugawawawawawawawawawawawawawawawawawawaw",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:post(CPid, Path, ?header_content_type_url_encoded, ?url_encode([{"key", Key}, {"value", Value}])), % post request
    {Status, _Headers, _Body} = test_util:receive_http_response(CPid, SRef),
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    ?assertEqual(413, Status), % check that status code has expected values
    ok.

%%--------------------------------------------------------------------
%% Function: test_update_key_provided_not_exists(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% 
%% Description: Negative test case for extraction of KV-pair from
%% cache storage engine (Key not found).
%%--------------------------------------------------------------------
test_update_key_provided_not_exists(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),
    Key = "shogun",
    Value = "tokugawa",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:put(CPid, Path ++ Key, ?header_content_type_url_encoded, ?url_encode([{"value", Value}])),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef), 
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
   
    ?assertEqual(404, Status), % check that status code has expected value
    ?assertEqual(jiffy:encode(#{key => list_to_binary(Key), error => <<"key not found">>}), Body),
    ok.

%%--------------------------------------------------------------------
%% Function: test_update_wrong_path(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for extraction of KV-pair from
%% cache storage engine (Key not found).
%%--------------------------------------------------------------------
test_update_wrong_path(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),    
    Key = "",
    Value = "tokugawa",

    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:put(CPid, Path ++ Key, ?header_content_type_url_encoded, ?url_encode([{"value", Value}])),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef), 
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(418, Status), % check that status code has expected value
    ?assertEqual(<<"{\"error\":\"you're a teapot ^_^\"}">>, Body),
    ok.

%%--------------------------------------------------------------------
%% Function: test_update_empty_body(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for update KV-pair in
%% storage, when key not provided in request.
%%--------------------------------------------------------------------
test_update_empty_body(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),
    Key = "shogun",
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:put(CPid, Path ++ Key, ?header_content_type_url_encoded, ""),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef), 
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(400, Status), % check that status code has expected value
    ?assertEqual(<<"{\"error\":\"wrong body format\"}">>, Body),
    ok.

%%--------------------------------------------------------------------
%% Function: test_get_key_provided_not_exists(Config0) -> ok.
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for extraction of KV-pair from
%% cache storage (Key not found).
%%--------------------------------------------------------------------
test_get_key_provided_not_exists(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),
    Key = "shogun",
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:get(CPid, Path ++ Key, ?header_content_type_url_encoded),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef), 
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(404, Status), % check that status code has expected value
    ?assertEqual(<<"{\"key\":\"shogun\",\"error\":\"key not found\"}">>, Body),
    ok.

%%--------------------------------------------------------------------
%% Function:  test_delete_key_not_provided((Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case when key to remove from
%% cache storage not provided.
%%--------------------------------------------------------------------
 test_delete_key_not_provided(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:delete(CPid, Path, ?header_content_type_url_encoded),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef),  
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(404, Status),  % check that status code has expected values
    ?assertEqual(<<"{\"error\":\"key not provided\"}">>, Body),
    ok.

%%--------------------------------------------------------------------
%% Function:  test_delete_key_provided_not_exists(Config0) -> ok. 
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% Description: Negative test case for removal of absent KV-pair from
%% cache storage.
%%--------------------------------------------------------------------
 test_delete_key_provided_not_exists(Config) -> 
    #{port := Port,
      path := Path,
      host := Host} = maps:from_list(Config),
    Key = "shogun",
    
    {ok, CPid} = gun:open(Host, Port), % initialize gun connection
    SRef = gun:delete(CPid, Path ++ Key, ?header_content_type_url_encoded),
    {Status, _Headers, Body} = test_util:receive_http_response(CPid, SRef),  
    gun:cancel(CPid, SRef), % canceling http stream
    gun:shutdown(CPid), % close gun connection
    
    ?assertEqual(404, Status), % check that status code has expected values
    ?assertEqual(<<"{\"key\":\"shogun\",\"error\":\"key not found\"}">>, Body),
    ok.