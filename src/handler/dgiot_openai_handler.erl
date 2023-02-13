%%--------------------------------------------------------------------
%% Copyright (c) 2020-2021 DGIOT Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------
-module(dgiot_openai_handler).
-author("stoneliu").
-behavior(dgiot_rest).
-dgiot_rest(all).
-include_lib("dgiot/include/logger.hrl").

%% API
-export([swagger_openai_tcp/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    dgiot_http_server:bind(<<"/pump">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    dgiot_http_server:bind(<<"/swagger_feeders.json">>, ?MODULE, [], priv)
swagger_openai_tcp() ->
    [
        dgiot_http_server:bind(<<"/swagger_openai.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: dgiot_req:req()) ->
    {Status :: dgiot_req:http_status(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: dgiot_req:http_status(), Headers :: map(), Body :: map(), Req :: dgiot_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            ?LOG(info, "do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false ->
                          dgiot_ctl:format("~p", [Reason])
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
%%            ?LOG(debug,"do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
%%            ?LOG(info,"do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% iot_hub 概要: 查询平台api资源 描述: 调用openai接口
%% OperationId:post_completions
%% 请求:GET /iotapi/
do_request(post_completions, Args, #{<<"sessionToken">> := _SessionToken} = _Context, _Req) ->
    io:format("Args = ~p.~n", [Args]),
    Url = "https://api.openai.com/v1/completions",
    Headers = [
        {"Authorization", "Bearer sk-"}
    ],
    Result =
        case httpc:request(post, {Url, Headers, "application/json", jsx:encode(Args)}, [], []) of
            {ok, {{"HTTP/1.1", 200, "OK"}, _, Json}} ->
                case jsx:decode(dgiot_utils:to_binary(Json), [{labels, binary}, return_maps]) of
                    #{<<"choices">> := _Choices} = Response ->
                        Response#{<<"code">> => <<"200">>};
                    Error ->
                        #{<<"code">> => <<"500">>, <<"error">> => Error}
                end;
            Error ->
                #{<<"code">> => <<"500">>, <<"error">> => Error}
        end,
    {ok, Result};

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    io:format("~s ~p _OperationId = ~p.~n", [?FILE, ?LINE, _OperationId]),
    io:format("~s ~p _Args = ~p.~n", [?FILE, ?LINE, _Args]),
%%    io:format("~s ~p _Context = ~p.~n", [?FILE, ?LINE, _Context]),
    {error, <<"Not Allowed.">>}.

