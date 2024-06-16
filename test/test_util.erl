-module(test_util).

-export([
            receive_http_response/2
        ]).

%%%-------------------------------------------------------------------
%% @doc 
%% helper functions for functional tests
%% @end
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-spec receive_http_response(CPid, SRef) -> Resp when
    CPid :: pid(),
    SRef :: gun:stream_ref(),
    Resp :: {non_neg_integer(), list(), string()}.
%% @doc 
%% Ensures safe conversion of term into JSON.
%% 
%% @end
%%%-------------------------------------------------------------------

receive_http_response(CPid, SRef) ->
    case gun:await(CPid, SRef) of
            {response, fin, Status, Headers} ->
                {Status, Headers, ""};
            {response, nofin, Status, Headers} ->
                {ok, Body} = gun:await_body(CPid, SRef),
                {Status, Headers, Body};
            _Response ->
                {0, [], ""}
    end.
