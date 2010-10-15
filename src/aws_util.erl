%%%
%%% Copyright (c) 2007 Bernhard H. Damberger 
%%% All rights reserved.
%%% 
%%% Developed by: 		Bernhard H. Damberger
%%%                     bernied at gmail dot com
%%%                     http://code.google.com/p/erlawys/
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the
%%% "Software"), to deal with the Software without restriction, including
%%% without limitation the rights to use, copy, modify, merge, publish,
%%% distribute, sublicense, and/or sell copies of the Software, and to
%%% permit persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%% 
%%% Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimers.
%%% Redistributions in binary form must reproduce the above
%%% copyright notice, this list of conditions and the following disclaimers
%%% in the documentation and/or other materials provided with the
%%% distribution.
%%% Neither the names of Bernhard H. Damberger,
%%% nor the names of its contributors may be used to endorse
%%% or promote products derived from this Software without specific prior
%%% written permission.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%% OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR
%%% ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS WITH THE SOFTWARE.
%%%

-module(aws_util).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/08/06').

-import(string, [to_lower/1]).

%-compile(export_all).
-export([filter_nulls/1, 
		params_signature/2, 
		add_default_params/3,
		create_ec2_param_list/2,
		tuple_3to2/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Methods used to construct URLs to access AWS.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remove nulls from parameter list.
filter_nulls([{_,null}|T]) -> filter_nulls(T);
filter_nulls([H|T]) -> [H|filter_nulls(T)];
filter_nulls([]) -> [].

% Function to sort params list.
sort_params(Params) -> lists:sort(fun({A, _}, {X, _}) -> to_lower(A) < to_lower(X) end, Params).

% Make sure you call crypto:start() before using this code.
params_signature(Key, Params) -> params_signature(Key, sort_params(Params), []).
params_signature(Key, [{K, V}|T], Data) -> params_signature(Key, T, [V,K|Data]);
params_signature(Key, [], Data) ->
    Signature = lists:flatten(lists:reverse(Data)), 
    base64:encode_to_string(crypto:sha_mac(Key, Signature)).

% Add default values to list.
%add_default_params(Params, AccessKey) -> add_default_params(Params, AccessKey, "2007-03-01").
add_default_params(Params, AccessKey, Version) ->
	NewParams = [{"AWSAccessKeyId", AccessKey},
	 {"Timestamp", create_timestamp()},
	 {"SignatureVersion", "1"},
	 {"Version", Version}
	| Params], sort_params(NewParams).
	
% Time utility function
add_zeros(L) -> if length(L) == 1 -> [$0|L]; true -> L end.
to_str(L) -> add_zeros(integer_to_list(L)).

create_timestamp() -> create_timestamp(calendar:now_to_universal_time(now())).
create_timestamp({{Y, M, D}, {H, Mn, S}}) ->
	to_str(Y) ++ "-" ++ to_str(M) ++ "-" ++ to_str(D) ++ "T" ++
	to_str(H) ++ ":" ++ to_str(Mn)++ ":" ++ to_str(S) ++ "Z".

% Create a list of name/value pairs for ec2 parameters.
create_ec2_param_list(Name, Params) -> create_ec2_param_list(Name, Params, 1).
create_ec2_param_list(_, null, _) -> [];
create_ec2_param_list(Name, [H|T], C) -> [{Name ++ "." ++ integer_to_list(C), H} | create_ec2_param_list(Name, T, C + 1)];
create_ec2_param_list(_, [], _) -> [].

% Strip 3 tuple to 2.
tuple_3to2({ok, X, _}) -> {ok, X};
tuple_3to2(T) -> T.

