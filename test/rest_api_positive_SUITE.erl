-module(rest_api_positive_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

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
    test_insert,
    test_update,
    test_get,
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
    Config.

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
init_per_testcase(test_insert, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    Client = "Rusuigumi", % define client name for ETS KV
    samurai_kv:connect(Client), % perform straight connection
    samurai_kv:insert(Client, <<"shogun">>, <<"tokugawa">>),
    samurai_kv:disconnect(Client), % perform straight connection
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
end_per_testcase(_TestCase, _Config) ->
    Client = "Rusuigumi", % define client name for ETS KV
    samurai_kv:connect(Client), % perform straight connection
    samurai_kv:delete(Client, <<"shogun">>),
    samurai_kv:disconnect(Client), % perform straight connection
    ok.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
 %% Function: test_insert(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %%
 %% Config0 = Config1 = [tuple()]
 %%   A list of key/value pairs, holding the test case configuration.
 %% Reason = term()
 %%   The reason for skipping the test case.
 %% Comment = term()
 %%   A comment about the test case that will be printed in the html log.
 %%
 %% Description: Positive test case for insertion of the KV-pair to 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_insert(_Config) -> 
    Client = "Shinobi", % define client name for ETS KV
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, <<"Client connected">>} = samurai_kv:connect(Client), % perform straight connection to cache core engine
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
    SRef = gun:post(CPid, "/cache",[{<<"content-type">>, "application/x-www-form-urlencoded"}],"key=shogun&value=tokugawa"), % post request
    {response, _, Status,_} =  gun:await(CPid, SRef), % receive the responce for POST request
    {ok, {_, Value}} = samurai_kv:get(Client, <<"shogun">>), % get posted value from cache  
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    {ok, <<"Client disconnected">>} = samurai_kv:disconnect(Client), % disconnect from kv storage
    ?assertEqual(204, Status), % check that status code has expected values
    ?assertEqual(<<"tokugawa">>,Value).  % check that value stored in cache is same as posted

%%--------------------------------------------------------------------
 %% Function: test_update(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %%
 %% Config0 = Config1 = [tuple()]
 %%   A list of key/value pairs, holding the test case configuration.
 %% Reason = term()
 %%   The reason for skipping the test case.
 %% Comment = term()
 %%   A comment about the test case that will be printed in the html log.
 %%
 %% Description: Positive test case for update of the value for KV-pair in
 %% cache storage engine.
 %%--------------------------------------------------------------------
 test_update(_Config) -> 
    Client = "Daimyo", % define client name for ETS KV
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, <<"Client connected">>} = samurai_kv:connect(Client), % perform straight connection to cache core engine
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
    SRef = gun:post(CPid, "/cache",[{<<"content-type">>, "application/x-www-form-urlencoded"}],"key=shogun&value=kamakura"), % post request
    {response, _, Status,_} =  gun:await(CPid, SRef), % receive the responce for POST request
    {ok, {_, Value}} = samurai_kv:get(Client, <<"shogun">>), % get posted value from cache  
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    {ok, <<"Client disconnected">>} = samurai_kv:disconnect(Client), % disconnect from kv storage
    ?assertEqual(204, Status), % check that status code has expected values
    ?assertEqual(<<"kamakura">>,Value).  % check that value stored in cache is same as posted

%%--------------------------------------------------------------------
 %% Function: test_get(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %%
 %% Config0 = Config1 = [tuple()]
 %%   A list of key/value pairs, holding the test case configuration.
 %% Reason = term()
 %%   The reason for skipping the test case.
 %% Comment = term()
 %%   A comment about the test case that will be printed in the html log.
 %%
 %% Descritption: Positive test case for extracton of KV-pair from
 %% cache storage engine.
 %%--------------------------------------------------------------------
 test_get(_Config) -> 
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
     % get request (with body)
    SRef = gun:request(CPid, <<"GET">>, "/cache", [{<<"content-type">>, "application/x-www-form-urlencoded"}], <<"key=shogun">>),
    {Status, Headers, Body} =case gun:await(CPid, SRef) of
        {response, fin, SW, Hdrs} ->
            {SW, Hdrs, ""};
        {response, nofin, SW, Hdrs} ->
            {ok, Answ} = gun:await_body(CPid, SRef),
            {SW, Hdrs, Answ}
    end,
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    ?assertEqual(200, Status), % check that status code has expected values
    ?assertEqual(<<"{\"shogun\":\"tokugawa\"}">>, Body), % check that responce body has expected form 
    ?assert(lists:member({<<"content-type">>, <<"application/json">>},Headers)). %  check that returned content type is json

%%--------------------------------------------------------------------
 %% Function: test_delete(Config0) ->
 %%               ok | exit() | {skip,Reason} | {comment,Comment} |
 %%               {save_config,Config1} | {skip_and_save,Reason,Config1}
 %%
 %% Config0 = Config1 = [tuple()]
 %%   A list of key/value pairs, holding the test case configuration.
 %% Reason = term()
 %%   The reason for skipping the test case.
 %% Comment = term()
 %%   A comment about the test case that will be printed in the html log.
 %%
 %% Descritption: Positive test case for removal of KV-pair from
 %% cache storage engine.
 %%--------------------------------------------------------------------
 test_delete(_Config) -> 
    Client = "Umamawari", % define client name for ETS KV
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, <<"Client connected">>} = samurai_kv:connect(Client), % perform straight connection to cache core engine
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
    % get request
    SRef = gun:request(CPid, <<"DELETE">>, "/cache", [{<<"content-type">>, "application/x-www-form-urlencoded"}], <<"key=shogun">>),
    {response, _, Status,_} =  gun:await(CPid, SRef), % receive the responce for DELETE request
    {error, Reason} = samurai_kv:get(Client, <<"tom">>), % get posted value from cache  
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    {ok, <<"Client disconnected">>} = samurai_kv:disconnect(Client), % disconnect from kv storage
    ?assertEqual(204, Status), % check that status code has expected values
    ?assertEqual(<<"Key doesn't exist">>, Reason).  % check that value stored in cache is same as posted