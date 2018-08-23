%% Copyright 2017 Pinterest, Inc.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

Definitions.

WHITESPACE      = [\s\t\r\n]+
COMMENT         = //[^\n]*
CCOMMENT        = /\*/*([^*/]|[^*]/|\*[^/])*\**\*/
COMMENTS        = {COMMENT}|{CCOMMENT}
UNIXCOMMENT     = #[^\n]*

INT             = [+-]?[0-9]+
HEX             = [+-]?0x[0-9A-Fa-f]+
BADDOUBLE       = [+-]?[0-9]+[eE][+-]?[0-9]+
DOUBLE          = [+-]?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?
PUNCTUATOR      = [\{\}\[\]\(\)<>,:;=\*]
STRING          = '(\\'|[^\'])*'|"(\\"|[^\"])*"
BOOLEAN         = true|false

IDENTIFIER      = [a-zA-Z_](\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*

KEYWORDS1       = namespace|include|cpp_include
KEYWORDS2       = typedef|enum|union|struct|exception
KEYWORDS3       = void|bool|byte|i8|i16|i32|i64|double|string|binary|list|map|set
KEYWORDS4       = const|oneway|extends|throws|service|required|optional
KEYWORD         = {KEYWORDS1}|{KEYWORDS2}|{KEYWORDS3}|{KEYWORDS4}

RESERVED1       = BEGIN|END|__CLASS__|__DIR__|__FILE__|__FUNCTION__|__LINE__
RESERVED2       = __METHOD__|__NAMESPACE__|abstract|alias|and|args|as|assert|begin
RESERVED3       = break|case|catch|class|clone|continue|declare|def|default|del
RESERVED4       = delete|do|dynamic|elif|else|elseif|elsif|end|enddeclare|endfor
RESERVED5       = endforeach|endif|endswitch|endwhile|ensure|except|exec|finally
RESERVED6       = float|for|foreach|from|function|global|goto|if|implements|import
RESERVED7       = in|inline|instanceof|interface|is|lambda|module|native|new|next
RESERVED8       = nil|not|or|package|pass|public|print|private|protected|raise|redo
RESERVED9       = rescue|retry|register|return|self|sizeof|static|super|switch
RESERVED10      = synchronized|then|this|throw|transient|try|undef|unless|unsigned
RESERVED11      = until|use|var|virtual|volatile|when|while|with|xor|yield
RESERVED12      = {RESERVED1}|{RESERVED2}|{RESERVED3}|{RESERVED4}|{RESERVED5}
RESERVED13      = {RESERVED6}|{RESERVED7}|{RESERVED8}|{RESERVED9}|{RESERVED10}
RESERVED        = {RESERVED11}|{RESERVED12}|{RESERVED13}

Rules.

{WHITESPACE}    : skip_token.
{COMMENTS}      : skip_token.
{UNIXCOMMENT}   : process_unix_comment(TokenChars, TokenLine).

__file__        : {token, {file, TokenLine}}.
{PUNCTUATOR}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{KEYWORD}       : {token, {list_to_atom(TokenChars), TokenLine}}.

{INT}           : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{HEX}           : {token, {int, TokenLine, hex_to_integer(TokenChars)}}.
{DOUBLE}        : {token, {double, TokenLine, list_to_float(TokenChars)}}.
{BADDOUBLE}     : {token, {double, TokenLine, bad_list_to_float(TokenChars)}}.
{STRING}        : {token, {string, TokenLine, process_string(TokenChars, TokenLen)}}.
{BOOLEAN}       : {token, {list_to_atom(TokenChars), TokenLine}}.

{RESERVED}      : reserved_keyword_error(TokenChars, TokenLine).

{IDENTIFIER}    : {token, {ident, TokenLine, TokenChars}}.

Erlang code.

process_unix_comment("#@namespace" ++ Rest, Line) -> {token, {namespace, Line}, Rest};
process_unix_comment("#" ++ _Rest, _Line)         -> skip_token.

hex_to_integer([$+|Chars]) ->  hex_to_integer(Chars);
hex_to_integer([$-|Chars]) -> -hex_to_integer(Chars);
hex_to_integer([$0,$x|Chars]) -> list_to_integer(Chars, 16).

% Erlang/Elixir can not parse integer significand in a float, so handle this case.

bad_list_to_float(BadFloat) ->
    {Significand, Rest} = string:to_integer(BadFloat),
    GoodFloat = io_lib:format("~b.0~s", [Significand, Rest]),
    list_to_float(lists:flatten(GoodFloat)).

% Process a quoted string by stripping its surrounding quote characters and
% expanding any escape sequences (prefixed by a \). To keep things simple,
% we're very lenient in that we allow any character to be escaped, and if the
% character isn't "special" (like \n), we just return the unescaped character.
% It might be nicer in the future to report "bad" escape characters, but that
% would involve complicating this logic to allow a top-level {error, Reason}
% result that could be returned to leex above.

process_string(S,Len)           -> process_chars(lists:sublist(S, 2, Len-2)).
process_chars([$\\,$n|Chars])   -> [$\n|process_chars(Chars)];
process_chars([$\\,$r|Chars])   -> [$\r|process_chars(Chars)];
process_chars([$\\,$t|Chars])   -> [$\t|process_chars(Chars)];
process_chars([$\\,C|Chars])    -> [C|process_chars(Chars)];
process_chars([C|Chars])        -> [C|process_chars(Chars)];
process_chars([])               -> [].

reserved_keyword_error(Keyword, _Line) ->
    Message = io_lib:format(
      "cannot use reserved language keyword \"~s\"", [Keyword]),
    {error, Message}.