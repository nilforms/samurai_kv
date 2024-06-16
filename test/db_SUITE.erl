-module(db_SUITE).

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
        test_db_basic_functions_positive,
        test_db_add_twice_same_key_negative,
        test_db_storage_full,
        test_db_get_key_negative,
        test_db_update_key_negative,
        test_db_get_all_keys_negative,
        test_db_too_many_connections
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

%--------------------------------------------------------------------
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
test_db_basic_functions_positive(_Config) -> 
    Key1  = <<"shogun">>,
    Val11 = <<"tokugawa">>,
    Val12 = <<"asuka">>,
    Key2  = <<"daimyo">>,
    Val2  = <<"yasui">>,

    %% test adding the keys
    ResAdd1 = samurai_kv:add(Key1, Val11),
    ?assertEqual(Key1, maps:get(key, ResAdd1)),
    ?assertEqual(<<"key added">>, maps:get(message, ResAdd1)),
    ResAdd2 = samurai_kv:add(Key2, Val2),
    ?assertEqual(Key2, maps:get(key, ResAdd2)),
    ?assertEqual(<<"key added">>, maps:get(message, ResAdd2)),
    
    %% test getting single key
    ResGet1 = samurai_kv:get(Key1),
    ?assertEqual(#{key => Key1, value => Val11}, ResGet1),
    ResGet2 = samurai_kv:get(Key2),
    ?assertEqual(#{key => Key2, value => Val2}, ResGet2),

    %% test getting all the keys
    ResGetAll = samurai_kv:get_all(),
    ?assert(lists:member(#{key => Key1, value => Val11}, ResGetAll)),
    ?assert(lists:member(#{key => Key2, value => Val2}, ResGetAll)),
    
    %% test updating the Key1
    ResUpd = samurai_kv:update(Key1, Val12),
    ?assertEqual(Key1, maps:get(key, ResUpd)),
    ?assertEqual(<<"key updated">>, maps:get(message, ResUpd)),
    ResGetUpd = samurai_kv:get(Key1),
    ?assertEqual(#{key => Key1, value => Val12}, ResGetUpd),

    %% test removing the keys
    ResDel1 = samurai_kv:delete(Key1),
    ?assertEqual(Key1, maps:get(key, ResDel1)),
    ?assertEqual(<<"key deleted">>, maps:get(message, ResDel1)),
    ResDel2 = samurai_kv:delete(Key2),
    ?assertEqual(Key2, maps:get(key, ResDel2)),
    ?assertEqual(<<"key deleted">>, maps:get(message, ResDel2)),

    ok.

%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_add_twice_same_key_negative(_Config) -> 
    Key  = <<"shogun">>,
    Val1 = <<"tokugawa">>,
    Val2 = <<"asuka">>,

    %% adding the key first time
    ResAdd1 = samurai_kv:add(Key, Val1),
    ?assertEqual(Key, maps:get(key, ResAdd1)),
    ?assertEqual(<<"key added">>, maps:get(message, ResAdd1, "")),
    %% trying to add the same key twice
    ResAdd2 = samurai_kv:add(Key, Val2),
    ?assertEqual(Key, maps:get(key, ResAdd2)),
    ?assertEqual(<<"key already exists">>, maps:get(error, ResAdd2, "")),
        
    samurai_kv:delete(Key),
    ok.

%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_storage_full(_Config) -> 
    Key1  = <<"shogun">>,
    Val1  = <<"tokugawa">>,
    Key2  = <<"daimyo">>,
    Val2  = <<"yasui">>,
    Key3  = <<"rurounin">>,
    Val3  = <<"kenshin">>,
    
    samurai_kv:add(Key1, Val1),
    samurai_kv:add(Key2, Val2),

    ResAdd = samurai_kv:add(Key3, Val3),
    ?assertEqual(<<"storage size limit exceeded">>, maps:get(error, ResAdd, "")),
    
    samurai_kv:delete(Key1),
    samurai_kv:delete(Key2),
    ok.


%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_get_key_negative(_Config) -> 
    Key  = <<"shogun">>,
    ResGet = samurai_kv:get(Key),
    ?assertEqual(Key, maps:get(key, ResGet)),
    ?assertEqual(<<"key not found">>, maps:get(error, ResGet)),
    ok.

%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_update_key_negative(_Config) -> 
    Key = <<"shogun">>,
    Val = <<"asuka">>,
    ResUpd = samurai_kv:update(Key, Val),
    ?assertEqual(Key, maps:get(key, ResUpd)),
    ?assertEqual(<<"key not found">>, maps:get(error, ResUpd)),
    ok.

%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_get_all_keys_negative(_Config) -> 
    ResGet = samurai_kv:get_all(),
    ?assertEqual(<<"storage empty">>, maps:get(message, ResGet)),
    ok.

%%--------------------------------------------------------------------
 %% Function: test_db_add_twice_same_key_neagtive(Config) -> ok
 %%
 %%
 %% Description: Negative test case for insertion/update of the KV-pair in 
 %% cache storage engine.
 %%--------------------------------------------------------------------
test_db_too_many_connections(_Config) -> 
    Key = <<"shogun">>,
    Self = self(), 
    Pids = [spawn_link(fun() -> Self ! {self(), samurai_kv:delete(Key)} end) || _I <- lists:seq(1,2)],
    Results = [receive {Pid, R} -> R end|| Pid <-Pids],
    ResDel = lists:last(Results),
    ?assertEqual(<<"too many connections">>, maps:get(error, ResDel)),
    ok.