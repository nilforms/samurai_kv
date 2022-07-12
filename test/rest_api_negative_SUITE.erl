-module(rest_api_negative_SUITE).
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
    test_insert_update,
    test_get1,
    test_get2 ,
    test_delete
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
end_per_testcase(_TestCase, _Config) ->
    ok.


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
 %% Function: test_insert_update(Config0) ->
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
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_insert_update(_Config) -> 
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
    SRef = gun:post(CPid, "/cache",[{<<"content-type">>, "application/x-www-form-urlencoded"}],""), % post request
    % receive the responce for POST request
    {Status, Headers, Body} =case gun:await(CPid, SRef) of
        {response, fin, SW, Hdrs} ->
            {SW, Hdrs, ""};
        {response, nofin, SW, Hdrs} ->
            {ok, Answ} = gun:await_body(CPid, SRef),
            {SW, Hdrs, Answ}
    end,  
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    ?assertEqual(400, Status), % check that status code has expected values
    ?assertEqual(<<"{\"error\":\"Bad Request\"}">>, Body), % check that responce body has expected form 
    ?assert(lists:member({<<"content-type">>, <<"application/json">>},Headers)). %  check that returned content type is json

%%--------------------------------------------------------------------
 %% Function: test_get1(Config0) ->
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
 %% Descritption: Negative test case for extracton of KV-pair from
 %% cache storage engine (Bad request).
 %%--------------------------------------------------------------------
 test_get1(_Config) -> 
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
     % get request (with body)
    SRef = gun:get(CPid, "/cache", [{<<"content-type">>, "application/x-www-form-urlencoded"}]),
    {Status, Headers, Body} =case gun:await(CPid, SRef) of
        {response, fin, SW, Hdrs} ->
            {SW, Hdrs, ""};
        {response, nofin, SW, Hdrs} ->
            {ok, Answ} = gun:await_body(CPid, SRef),
            {SW, Hdrs, Answ}
    end,
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    ?assertEqual(400, Status), % check that status code has expected values
    ?assertEqual(<<"{\"error\":\"Bad Request\"}">>, Body), % check that responce body has expected form 
    ?assert(lists:member({<<"content-type">>, <<"application/json">>},Headers)). %  check that returned content type is json

%%--------------------------------------------------------------------
 %% Function: test_get1(Config0) ->
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
 %% Descritption: Negative test case for extracton of KV-pair from
 %% cache storage engine (Key not found).
 %%--------------------------------------------------------------------
test_get2(_Config) -> 
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
     % get request (with body)
    SRef = gun:request(CPid, <<"GET">>, "/cache", [{<<"content-type">>, "application/x-www-form-urlencoded"}], <<"key=shogun">>),
    {response, _, Status,_} =  gun:await(CPid, SRef), % receive the responce for DELETE request
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    ?assertEqual(404, Status).% check that status code has expected value

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
 %% Descritption: Negative test case for removal of KV-pair from
 %% cache storage engine.
 %%--------------------------------------------------------------------
 test_delete(_Config) -> 
    {ok, Port} = application:get_env(samurai_kv_http_api, http_port), % get the pre-defined server tcp port
    {ok, CPid} = gun:open("localhost", Port), % initialize gun connection
    % delete request
    SRef = gun:delete(CPid, "/cache", [{<<"content-type">>, "application/x-www-form-urlencoded"}]),
    {Status, Headers, Body} =case gun:await(CPid, SRef) of
        {response, fin, SW, Hdrs} ->
            {SW, Hdrs, ""};
        {response, nofin, SW, Hdrs} ->
            {ok, Answ} = gun:await_body(CPid, SRef),
            {SW, Hdrs, Answ}
    end, % receive the responce for DELETE request
    gun:cancel(CPid,SRef), % canceling http stream
    gun:close(CPid), % close gun connection
    ?assertEqual(400, Status), % check that status code has expected values
    ?assertEqual(<<"{\"error\":\"Bad Request\"}">>, Body), % check that responce body has expected form 
    ?assert(lists:member({<<"content-type">>, <<"application/json">>},Headers)). %  check that returned content type is json