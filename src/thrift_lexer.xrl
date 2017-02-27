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
IGNORED         = [;]
COMMENT         = //[^\n]*
CCOMMENT        = /\*/*([^*/]|[^*]/|\*[^/])*\**\*/
UNIXCOMMENT     = #[^\n]*
COMMENTS        = {COMMENT}|{CCOMMENT}|{UNIXCOMMENT}

INT             = [+-]?[0-9]+
HEX             = [+-]?0x[0-9A-Fa-f]+
DOUBLE          = [+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?
PUNCTUATOR      = [\{\}\[\]\(\)<>,:=\*]
STRING          = '(\\'|[^\'])*'|"(\\"|[^\"])*"
BOOLEAN         = true|false

IDENTIFIER      = [a-zA-Z_](\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*

KEYWORDS1       = namespace|include
KEYWORDS2       = typedef|enum|union|struct|exception
KEYWORDS3       = void|bool|byte|i8|i16|i32|i64|double|string|binary|list|map|set
KEYWORDS4       = const|oneway|extends|throws|service|required|optional
KEYWORD         = {KEYWORDS1}|{KEYWORDS2}|{KEYWORDS3}|{KEYWORDS4}

Rules.

{WHITESPACE}    : skip_token.
{IGNORED}       : skip_token.
{COMMENTS}      : skip_token.

__file__        : {token, {file, TokenLine}}.
{PUNCTUATOR}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{KEYWORD}       : {token, {list_to_atom(TokenChars), TokenLine}}.

{INT}           : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{HEX}           : {token, {int, TokenLine, hex_to_integer(TokenChars)}}.
{DOUBLE}        : {token, {double, TokenLine, list_to_float(TokenChars)}}.
{STRING}        : {token, {string, TokenLine, process_string(TokenChars, TokenLen)}}.
{BOOLEAN}       : {token, {list_to_atom(TokenChars), TokenLine}}.

{IDENTIFIER}    : {token, {ident, TokenLine, TokenChars}}.

Erlang code.

hex_to_integer([$+|Chars]) ->  hex_to_integer(Chars);
hex_to_integer([$-|Chars]) -> -hex_to_integer(Chars);
hex_to_integer([$0,$x|Chars]) -> list_to_integer(Chars, 16).

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
