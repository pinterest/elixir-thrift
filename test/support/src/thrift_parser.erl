-module(thrift_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/thrift_parser.yrl", 213).

unwrap({V, _}) -> V;
unwrap({_,_,V}) -> V.

-file("/usr/local/Cellar/erlang/18.3/lib/erlang/lib/parsetools-2.1.1/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/thrift_parser.erl", 182).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, include, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, namespace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_4(4, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_header(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_header(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, exception, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, typedef, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2_20(20, Cat, [4 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_5(S, include, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, namespace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccpars2_13(_S, Cat, [5 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).
yeccpars2_6(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_7/7}).
yeccpars2_7(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_8/7}).
yeccpars2_8(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_ns_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_ns_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_namespace_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_include_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_headers(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(S, file, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2_153(_S, Cat, [20 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_21(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, exception, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, service, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, typedef, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2_152(_S, Cat, [21 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_definition(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
yeccpars2_23(S, binary, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, byte, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, i16, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, i32, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, i64, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, i8, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, list, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, map, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, set, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_24/7}).
yeccpars2_24(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
yeccpars2_25(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
yeccpars2_26(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_27/7}).
yeccpars2_27(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_28: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_29/7}).
yeccpars2_29(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).
yeccpars2_30(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_31(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, binary, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_binary(Stack),
 yeccpars2_34(34, binary, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, bool, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_bool(Stack),
 yeccpars2_34(34, bool, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, byte, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_byte(Stack),
 yeccpars2_34(34, byte, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_double(Stack),
 yeccpars2_34(34, double, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, i16, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_i16(Stack),
 yeccpars2_34(34, i16, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, i32, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_i32(Stack),
 yeccpars2_34(34, i32, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, i64, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_i64(Stack),
 yeccpars2_34(34, i64, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, i8, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_i8(Stack),
 yeccpars2_34(34, i8, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, ident, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_ident(Stack),
 yeccpars2_34(34, ident, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, list, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_list(Stack),
 yeccpars2_34(34, list, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, map, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_map(Stack),
 yeccpars2_34(34, map, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, optional, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_optional(Stack),
 yeccpars2_34(34, optional, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, required, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_required(Stack),
 yeccpars2_34(34, required, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, set, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_set(Stack),
 yeccpars2_34(34, set, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_string(Stack),
 yeccpars2_34(34, string, [31 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2_32(32, Cat, [31 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2_96(96, Cat, [33 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_34(S, optional, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, required, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2_23(37, Cat, [34 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).
yeccpars2_35(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_field_id(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_37: see yeccpars2_23

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_field_required(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_field_required(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_40/7}).
yeccpars2_40(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
yeccpars2_54(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).
yeccpars2_55(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_56/7}).
yeccpars2_56(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_base_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_58: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_59/7}).
yeccpars2_59(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_set_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_62/7}).
yeccpars2_62(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_63: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_64/7}).
yeccpars2_64(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_map_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_66: see yeccpars2_23

-dialyzer({nowarn_function, yeccpars2_67/7}).
yeccpars2_67(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_list_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_69(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2_70(_S, Cat, [69 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_field_spec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).
yeccpars2_71(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, false, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, true, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_field_default(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_73: see yeccpars2_71

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_79: see yeccpars2_71

-dialyzer({nowarn_function, yeccpars2_80/7}).
yeccpars2_80(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_81(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_mappings(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_82/7}).
yeccpars2_82(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_82(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_83(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_literal_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_84: see yeccpars2_71

%% yeccpars2_85: see yeccpars2_71

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_mapping(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_literal_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_literal_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_89_(Stack),
 yeccgoto_literal(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_71

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_91_(Stack),
 yeccgoto_mappings(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_92/7}).
yeccpars2_92(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_literal(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_94/7}).
yeccpars2_94(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_literal(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_96_)'(Stack),
 yeccpars2_98(_S, ')', [96 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_96(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_96_}'(Stack),
 yeccpars2_98(_S, '}', [96 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccpars2_34(34, Cat, [96 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_field_sep(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_fields(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_union_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).
yeccpars2_100(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_typedef_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_102/7}).
yeccpars2_102(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_103(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, binary, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_binary(Stack),
 yeccpars2_34(34, binary, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, bool, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_bool(Stack),
 yeccpars2_34(34, bool, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, byte, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_byte(Stack),
 yeccpars2_34(34, byte, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_double(Stack),
 yeccpars2_34(34, double, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, i16, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_i16(Stack),
 yeccpars2_34(34, i16, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, i32, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_i32(Stack),
 yeccpars2_34(34, i32, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, i64, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_i64(Stack),
 yeccpars2_34(34, i64, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, i8, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_i8(Stack),
 yeccpars2_34(34, i8, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, ident, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_ident(Stack),
 yeccpars2_34(34, ident, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, list, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_list(Stack),
 yeccpars2_34(34, list, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, map, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_map(Stack),
 yeccpars2_34(34, map, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, optional, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_optional(Stack),
 yeccpars2_34(34, optional, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, required, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_required(Stack),
 yeccpars2_34(34, required, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, set, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_set(Stack),
 yeccpars2_34(34, set, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_string(Stack),
 yeccpars2_34(34, string, [103 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccpars2_104(104, Cat, [103 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_104/7}).
yeccpars2_104(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_struct_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(S, extends, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccpars2_107(107, Cat, [106 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).
yeccpars2_107(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_108/7}).
yeccpars2_108(S, extends, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_109/7}).
yeccpars2_109(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_service_extends(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_111/7}).
yeccpars2_111(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_112/7}).
yeccpars2_112(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_service_extends(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, oneway, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, binary, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_binary(Stack),
 yeccpars2_115(115, binary, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, bool, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_bool(Stack),
 yeccpars2_115(115, bool, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, byte, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_byte(Stack),
 yeccpars2_115(115, byte, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_double(Stack),
 yeccpars2_115(115, double, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, i16, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_i16(Stack),
 yeccpars2_115(115, i16, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, i32, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_i32(Stack),
 yeccpars2_115(115, i32, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, i64, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_i64(Stack),
 yeccpars2_115(115, i64, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, i8, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_i8(Stack),
 yeccpars2_115(115, i8, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, ident, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_ident(Stack),
 yeccpars2_115(115, ident, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, list, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_list(Stack),
 yeccpars2_115(115, list, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, map, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_map(Stack),
 yeccpars2_115(115, map, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, set, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_set(Stack),
 yeccpars2_115(115, set, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_string(Stack),
 yeccpars2_115(115, string, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, void, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_void(Stack),
 yeccpars2_115(115, void, [114 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccpars2_116(116, Cat, [114 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_115(S, void, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_116/7}).
yeccpars2_116(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_116(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_117(S, oneway, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, binary, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_binary(Stack),
 yeccpars2_115(115, binary, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, bool, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_bool(Stack),
 yeccpars2_115(115, bool, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, byte, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_byte(Stack),
 yeccpars2_115(115, byte, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_double(Stack),
 yeccpars2_115(115, double, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, i16, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_i16(Stack),
 yeccpars2_115(115, i16, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, i32, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_i32(Stack),
 yeccpars2_115(115, i32, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, i64, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_i64(Stack),
 yeccpars2_115(115, i64, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, i8, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_i8(Stack),
 yeccpars2_115(115, i8, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, ident, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_ident(Stack),
 yeccpars2_115(115, ident, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, list, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_list(Stack),
 yeccpars2_115(115, list, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, map, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_map(Stack),
 yeccpars2_115(115, map, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, set, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_set(Stack),
 yeccpars2_115(115, set, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_string(Stack),
 yeccpars2_115(115, string, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, void, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_void(Stack),
 yeccpars2_115(115, void, [117 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccpars2_119(_S, Cat, [117 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_oneway_marker(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_functions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_service_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_return_type(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_122/7}).
yeccpars2_122(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_return_type(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_124/7}).
yeccpars2_124(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_125(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_)'(Stack),
 yeccpars2_126(126, ')', [125 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_(Stack),
 yeccpars2_34(34, Cat, [125 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_126/7}).
yeccpars2_126(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_127(S, throws, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 yeccpars2_128(128, Cat, [127 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_128(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_128_(Stack),
 yeccpars2_133(_S, Cat, [128 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_129/7}).
yeccpars2_129(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_130(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_130_)'(Stack),
 yeccpars2_131(131, ')', [130 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccpars2_34(34, Cat, [130 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
yeccpars2_131(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_throws_clause(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_function(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_134/7}).
yeccpars2_134(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_135(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_S, binary, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_binary(Stack),
 yeccpars2_34(34, binary, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, bool, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_bool(Stack),
 yeccpars2_34(34, bool, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, byte, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_byte(Stack),
 yeccpars2_34(34, byte, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_double(Stack),
 yeccpars2_34(34, double, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, i16, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_i16(Stack),
 yeccpars2_34(34, i16, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, i32, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_i32(Stack),
 yeccpars2_34(34, i32, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, i64, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_i64(Stack),
 yeccpars2_34(34, i64, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, i8, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_i8(Stack),
 yeccpars2_34(34, i8, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, ident, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_ident(Stack),
 yeccpars2_34(34, ident, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, list, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_list(Stack),
 yeccpars2_34(34, list, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, map, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_map(Stack),
 yeccpars2_34(34, map, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, optional, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_optional(Stack),
 yeccpars2_34(34, optional, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, required, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_required(Stack),
 yeccpars2_34(34, required, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, set, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_set(Stack),
 yeccpars2_34(34, set, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_string(Stack),
 yeccpars2_34(34, string, [135 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccpars2_136(136, Cat, [135 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_136/7}).
yeccpars2_136(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_exception_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_138/7}).
yeccpars2_138(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_139/7}).
yeccpars2_139(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_140(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccpars2_146(146, Cat, [140 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_141/7}).
yeccpars2_141(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_142(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_enum_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_143/7}).
yeccpars2_143(S, int, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_enum_value(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_145_(Stack),
 yeccgoto_enum_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_146(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 yeccgoto_enum_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_enum_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_148/7}).
yeccpars2_148(S, ident, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_149/7}).
yeccpars2_149(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_150: see yeccpars2_71

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 yeccgoto_constant_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_definitions(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_schema(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_154/7}).
yeccpars2_154(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_154(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_file_def(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_base_type(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_base_type(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_constant_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_constant_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_definition(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_definition(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(21, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_definitions(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(20, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_definitions(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_body(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(141, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_body(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_enum_value(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_enum_value(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_exception_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_exception_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_default(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_id(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_id(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_id(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_id(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_id(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_id(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_required(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(37, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_sep(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_sep(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_sep(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_spec(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_spec(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_spec(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_spec(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_spec(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_field_spec(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_fields(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fields(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fields(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fields(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(126, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fields(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_fields(135, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_file_def(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_functions(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(116, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_functions(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_header(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_header(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_headers(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_headers(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_include_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_include_def(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list_type(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_type(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_literal(71=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(84, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_literal_list(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal_list(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(82, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_literal_list(84=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_map_type(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_type(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mapping(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mapping(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mappings(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mappings(90=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_namespace_def(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_namespace_def(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ns_name(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_oneway_marker(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_oneway_marker(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return_type(115, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_schema(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_service_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_service_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_service_extends(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_type(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(61=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(66=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_type(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_struct_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_struct_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_throws_clause(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_type(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(59, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(62, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_type(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_typedef_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_typedef_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_union_def(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_union_def(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/thrift_parser.yrl", 66).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_4_/1}).
-file("src/thrift_parser.yrl", 72).
yeccpars2_4_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_5_/1}).
-file("src/thrift_parser.yrl", 66).
yeccpars2_5_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_9_/1}).
-file("src/thrift_parser.yrl", 115).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "*"
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/thrift_parser.yrl", 116).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("src/thrift_parser.yrl", 109).
yeccpars2_11_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Namespace' : new ( __2 , unwrap ( __3 ) )
  end | __Stack].

-compile({inline,yeccpars2_12_/1}).
-file("src/thrift_parser.yrl", 105).
yeccpars2_12_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Include' : new ( unwrap ( __2 ) )
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/thrift_parser.yrl", 67).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __2
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("src/thrift_parser.yrl", 62).
yeccpars2_20_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_21_/1}).
-file("src/thrift_parser.yrl", 72).
yeccpars2_21_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_31_binary/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_binary(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_bool/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_bool(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_byte/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_byte(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_double/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_double(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_i16/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_i16(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_i32/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_i32(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_i64/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_i64(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_i8/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_i8(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_ident/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_ident(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_list/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_list(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_map/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_map(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_optional/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_optional(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_required/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_required(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_set/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_set(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_string/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_31_string(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_31_/1}).
-file("src/thrift_parser.yrl", 153).
yeccpars2_31_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_33_/1}).
-file("src/thrift_parser.yrl", 137).
yeccpars2_33_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_34_/1}).
-file("src/thrift_parser.yrl", 165).
yeccpars2_34_(__Stack0) ->
 [begin
   default
  end | __Stack0].

-compile({inline,yeccpars2_36_/1}).
-file("src/thrift_parser.yrl", 156).
yeccpars2_36_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("src/thrift_parser.yrl", 164).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("src/thrift_parser.yrl", 163).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("src/thrift_parser.yrl", 84).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { set , __1 }
  end | __Stack].

-compile({inline,yeccpars2_42_/1}).
-file("src/thrift_parser.yrl", 85).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { map , __1 }
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/thrift_parser.yrl", 86).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { list , __1 }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/thrift_parser.yrl", 97).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   binary
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/thrift_parser.yrl", 89).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bool
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/thrift_parser.yrl", 90).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   i8
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/thrift_parser.yrl", 95).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   double
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-file("src/thrift_parser.yrl", 92).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   i16
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-file("src/thrift_parser.yrl", 93).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   i32
  end | __Stack].

-compile({inline,yeccpars2_51_/1}).
-file("src/thrift_parser.yrl", 94).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   i64
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("src/thrift_parser.yrl", 91).
yeccpars2_52_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   i8
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("src/thrift_parser.yrl", 87).
yeccpars2_53_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.StructRef' : new ( unwrap ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_57_/1}).
-file("src/thrift_parser.yrl", 96).
yeccpars2_57_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   string
  end | __Stack].

-compile({inline,yeccpars2_60_/1}).
-file("src/thrift_parser.yrl", 99).
yeccpars2_60_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_65_/1}).
-file("src/thrift_parser.yrl", 100).
yeccpars2_65_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __3 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_68_/1}).
-file("src/thrift_parser.yrl", 101).
yeccpars2_68_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/thrift_parser.yrl", 167).
yeccpars2_69_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_70_/1}).
-file("src/thrift_parser.yrl", 161).
yeccpars2_70_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Field' : new ( __1 , __2 , __3 , unwrap ( __4 ) , __5 )
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("src/thrift_parser.yrl", 168).
yeccpars2_72_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_74_/1}).
-file("src/thrift_parser.yrl", 131).
yeccpars2_74_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_75_/1}).
-file("src/thrift_parser.yrl", 129).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("src/thrift_parser.yrl", 130).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_77_/1}).
-file("src/thrift_parser.yrl", 132).
yeccpars2_77_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("src/thrift_parser.yrl", 128).
yeccpars2_78_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("src/thrift_parser.yrl", 121).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("src/thrift_parser.yrl", 125).
yeccpars2_83_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("src/thrift_parser.yrl", 120).
yeccpars2_86_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/thrift_parser.yrl", 126).
yeccpars2_87_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("src/thrift_parser.yrl", 125).
yeccpars2_88_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_89_/1}).
-file("src/thrift_parser.yrl", 133).
yeccpars2_89_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("src/thrift_parser.yrl", 122).
yeccpars2_91_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("src/thrift_parser.yrl", 134).
yeccpars2_93_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_95_/1}).
-file("src/thrift_parser.yrl", 135).
yeccpars2_95_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,'yeccpars2_96_)'/1}).
-file("src/thrift_parser.yrl", 153).
'yeccpars2_96_)'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_96_}'/1}).
-file("src/thrift_parser.yrl", 153).
'yeccpars2_96_}'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_96_/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_96_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_97_/1}).
-file("src/thrift_parser.yrl", 138).
yeccpars2_97_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   nil
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("src/thrift_parser.yrl", 154).
yeccpars2_98_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/thrift_parser.yrl", 184).
yeccpars2_99_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Union' : new ( unwrap ( __2 ) , __4 )
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("src/thrift_parser.yrl", 172).
yeccpars2_101_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { typedef , __2 , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_103_binary/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_binary(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_bool/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_bool(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_byte/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_byte(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_double/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_double(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_i16/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_i16(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_i32/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_i32(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_i64/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_i64(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_i8/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_i8(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_ident/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_ident(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_list/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_list(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_map/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_map(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_optional/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_optional(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_required/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_required(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_set/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_set(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_string/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_103_string(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_103_/1}).
-file("src/thrift_parser.yrl", 153).
yeccpars2_103_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_/1}).
-file("src/thrift_parser.yrl", 176).
yeccpars2_105_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Struct' : new ( unwrap ( __2 ) , __4 )
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("src/thrift_parser.yrl", 188).
yeccpars2_106_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_110_/1}).
-file("src/thrift_parser.yrl", 187).
yeccpars2_110_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_113_/1}).
-file("src/thrift_parser.yrl", 186).
yeccpars2_113_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __3 )
  end | __Stack].

-compile({inline,yeccpars2_114_binary/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_binary(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_bool/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_bool(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_byte/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_byte(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_double/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_double(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_i16/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_i16(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_i32/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_i32(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_i64/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_i64(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_i8/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_i8(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_ident/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_ident(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_list/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_list(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_map/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_map(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_set/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_set(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_string/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_string(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_void/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_114_void(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_114_/1}).
-file("src/thrift_parser.yrl", 191).
yeccpars2_114_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_117_binary/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_binary(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_bool/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_bool(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_byte/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_byte(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_double/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_double(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_i16/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_i16(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_i32/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_i32(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_i64/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_i64(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_i8/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_i8(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_ident/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_ident(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_list/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_list(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_map/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_map(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_set/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_set(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_string/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_string(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_void/1}).
-file("src/thrift_parser.yrl", 198).
yeccpars2_117_void(__Stack0) ->
 [begin
   false
  end | __Stack0].

-compile({inline,yeccpars2_117_/1}).
-file("src/thrift_parser.yrl", 191).
yeccpars2_117_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_118_/1}).
-file("src/thrift_parser.yrl", 199).
yeccpars2_118_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("src/thrift_parser.yrl", 192).
yeccpars2_119_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __2
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("src/thrift_parser.yrl", 180).
yeccpars2_120_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Service' : new ( unwrap ( __2 ) , __5 , __3 )
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-file("src/thrift_parser.yrl", 201).
yeccpars2_123_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   void
  end | __Stack].

-compile({inline,'yeccpars2_125_)'/1}).
-file("src/thrift_parser.yrl", 153).
'yeccpars2_125_)'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_125_/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_125_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_127_/1}).
-file("src/thrift_parser.yrl", 204).
yeccpars2_127_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_128_/1}).
-file("src/thrift_parser.yrl", 137).
yeccpars2_128_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,'yeccpars2_130_)'/1}).
-file("src/thrift_parser.yrl", 153).
'yeccpars2_130_)'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_130_/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_130_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_132_/1}).
-file("src/thrift_parser.yrl", 207).
yeccpars2_132_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("src/thrift_parser.yrl", 196).
yeccpars2_133_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Function' : new ( __1 , __2 , unwrap ( __3 ) , __5 , __7 )
  end | __Stack].

-compile({inline,yeccpars2_135_binary/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_binary(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_bool/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_bool(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_byte/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_byte(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_double/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_double(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_i16/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_i16(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_i32/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_i32(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_i64/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_i64(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_i8/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_i8(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_ident/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_ident(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_list/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_list(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_map/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_map(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_optional/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_optional(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_required/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_required(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_set/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_set(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_string/1}).
-file("src/thrift_parser.yrl", 157).
yeccpars2_135_string(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_135_/1}).
-file("src/thrift_parser.yrl", 153).
yeccpars2_135_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_137_/1}).
-file("src/thrift_parser.yrl", 151).
yeccpars2_137_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Exception' : new ( unwrap ( __2 ) , __4 )
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-file("src/thrift_parser.yrl", 137).
yeccpars2_140_(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,yeccpars2_142_/1}).
-file("src/thrift_parser.yrl", 146).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("src/thrift_parser.yrl", 147).
yeccpars2_144_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { unwrap ( __1 ) , unwrap ( __3 ) }
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-file("src/thrift_parser.yrl", 142).
yeccpars2_145_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.TEnum' : new ( unwrap ( __2 ) , __4 )
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-file("src/thrift_parser.yrl", 143).
yeccpars2_146_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_147_/1}).
-file("src/thrift_parser.yrl", 144).
yeccpars2_147_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_151_/1}).
-file("src/thrift_parser.yrl", 113).
yeccpars2_151_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Constant' : new ( unwrap ( __3 ) , __5 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_152_/1}).
-file("src/thrift_parser.yrl", 73).
yeccpars2_152_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 ] ++ __2
  end | __Stack].

-compile({inline,yeccpars2_153_/1}).
-file("src/thrift_parser.yrl", 60).
yeccpars2_153_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.Thrift.Parser.Models.Schema' : new ( __3 , __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("src/thrift_parser.yrl", 64).
yeccpars2_155_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   'Elixir.List' : to_string ( unwrap ( __2 ) )
  end | __Stack].


-file("src/thrift_parser.yrl", 217).
