%%--------------------------------------------------------------------
%% Copyright (c) 2020 DGIOT Technologies Co., Ltd. All Rights Reserved.
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

%% @doc dgiot_openai Protocol
-module(dgiot_openai).
-include("dgiot_openai.hrl").
-include_lib("dgiot/include/logger.hrl").

-export([do_requset/3
]).


-define(APP, ?MODULE).

do_requset(Type, Key, Args) ->
    Url = "https://api.openai.com/v1/" ++ dgiot_utils:to_list(Type),
    Authorization = "Bearer " ++ dgiot_utils:to_list(Key),
    Headers = [
        {"Authorization", Authorization}
    ],
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
    end.
