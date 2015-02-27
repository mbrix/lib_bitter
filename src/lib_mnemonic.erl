%
% [Ember Financial] INC ("COMPANY") CONFIDENTIAL
% Unpublished Copyright (c) 2014-2015 [Ember Financial, Inc], All Rights Reserved.
%
% NOTICE:  All information contained herein is, and remains the property of
% COMPANY. The intellectual and technical concepts contained herein are 
% proprietary to COMPANY and may be covered by U.S. and Foreign Patents, 
% patents in process, and are protected by trade secret or copyright law.
% Dissemination of this information or reproduction of this material is 
% strictly forbidden unless prior written permission is obtained from COMPANY.
% Access to the source code contained herein is hereby forbidden to anyone 
% except current COMPANY employees, managers or contractors who have executed 
% Confidentiality and Non-disclosure agreements explicitly covering such access.
%
% The copyright notice above does not evidence any actual or intended publication
% or disclosure  of  this source code, which includes information that is
% confidential and/or proprietary, and is a trade secret, of  COMPANY.  
% ANY REPRODUCTION, MODIFICATION, DISTRIBUTION, PUBLIC  PERFORMANCE, OR PUBLIC
% DISPLAY OF OR THROUGH USE  OF THIS  SOURCE CODE  WITHOUT  THE EXPRESS WRITTEN
% CONSENT OF COMPANY IS STRICTLY PROHIBITED, AND IN VIOLATION OF APPLICABLE 
% LAWS AND INTERNATIONAL TREATIES.  THE RECEIPT OR POSSESSION OF  THIS SOURCE
% CODE AND/OR RELATED INFORMATION DOES NOT CONVEY OR IMPLY ANY RIGHTS TO
% REPRODUCE, DISCLOSE OR DISTRIBUTE ITS CONTENTS, OR TO MANUFACTURE, USE, OR
% SELL ANYTHING THAT IT  MAY DESCRIBE, IN WHOLE OR IN PART.                
%
%

-module(lib_mnemonic).
%% BIP-0039 Implementation

-export([generate/1,
		 validate/1,
		 seed/1,
		 seed/2,
		 load_index/1]).

%% Generate a Mnemonic word list based on desired size
generate(128) -> generate_mnemonic(128);
generate(160) -> generate_mnemonic(160);
generate(192) -> generate_mnemonic(192);
generate(224) -> generate_mnemonic(224);
generate(256) -> generate_mnemonic(256).


generate_mnemonic(EntropyBits) ->
	InitialEntropy = crypto:strong_rand_bytes(EntropyBits div 8),
	Checksum = checksum(InitialEntropy),
	words(<<InitialEntropy/bitstring, Checksum/bitstring>>).

checksum(Bits) ->
	Size = size(Bits)*8 div 32,
	<<ChkSum:Size/bitstring, _Rest/bitstring>> = crypto:hash(sha256, Bits),
	ChkSum.

words(Bits) -> split_words(Bits, load_index(english), []).
split_words(<<>>, _WordList, MnemonicList) -> lists:reverse(MnemonicList);
split_words(<<WordIndex:11, Rest/bitstring>>, WordList, MnemonicList) ->
	split_words(Rest, WordList, [lookup_word(WordIndex+1, WordList)|MnemonicList]).

%% Convert word list to seed

seed(WordList) -> seed(WordList, <<>>).

seed(WordList, Password) ->
	Phrase = erlang:iolist_to_binary(WordList),
	{ok, Seed} = pbkdf2:pbkdf2({hmac, sha512}, Phrase,
							   <<"mnemonic", Password/bitstring>>, 2048),
	{ok, Seed}.

%% Validate a word list
%% Need the same word list the phrase was generated from
validate(Words) -> validate(Words, load_index(english)).
validate(Words, WordList) ->
	Bin = lists:foldl(fun(W, Acc) ->
					  {Index, _Word} = lists:keyfind(W, 2, WordList),
					  case Acc of
					  	  first -> <<(Index-1):11>>;
					  	  Other -> 
					  	  	  <<Other/bitstring, (Index-1):11>>
					  end
					  end, first, Words),
	verify_checksum(Bin).

verify_checksum(<<EntropyBits:128/bitstring, CSum:4/bitstring>>) ->
	vf_checksum(CSum, EntropyBits);
verify_checksum(<<EntropyBits:160/bitstring, CSum:5/bitstring>>) ->
	vf_checksum(CSum, EntropyBits);
verify_checksum(<<EntropyBits:192/bitstring, CSum:6/bitstring>>) ->
	vf_checksum(CSum, EntropyBits);
verify_checksum(<<EntropyBits:224/bitstring, CSum:7/bitstring>>) ->
	vf_checksum(CSum, EntropyBits);
verify_checksum(<<EntropyBits:256/bitstring, CSum:8/bitstring>>) ->
	vf_checksum(CSum, EntropyBits);
verify_checksum(_) -> false.

vf_checksum(CSum, EntropyBits) ->
	CSum =:= checksum(EntropyBits).

%% Utility
load_index(english) ->
	{ok, Data} = file:read_file(filename:join([code:priv_dir(lib_bitter),
											   <<"wordlists/">>, <<"english.txt">>])),
	WordList = binary:split(Data, [<<"\n">>], [global]),
	lists:zip(lists:seq(1, length(WordList)), WordList).


lookup_word(Index, WordList) ->
	{_Index, Word} = lists:nth(Index, WordList),
	Word.

