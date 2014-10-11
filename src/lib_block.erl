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

-module(lib_block).
-author('mbranton@emberfinancial.com').

-export([header/1,
         hashes/1,
         print/1,
        serialize/1]).

-include_lib("lib_bitter/include/bitter.hrl").

header(Block) when is_record(Block, bbdef) ->
	#bbdef{network=Block#bbdef.network,
		   blockhash=binary:copy(Block#bbdef.blockhash),
		   headerlength=Block#bbdef.headerlength,
		   version=Block#bbdef.version,
		   previoushash=binary:copy(Block#bbdef.previoushash),
		   merkleroot=binary:copy(Block#bbdef.merkleroot),
		   timestamp=Block#bbdef.timestamp,
		   difficulty=Block#bbdef.difficulty,
		   nonce=Block#bbdef.nonce,
		   txcount=Block#bbdef.txcount,
		   e_sumdiff=Block#bbdef.e_sumdiff,
		   e_height=Block#bbdef.e_height,
		   e_next=Block#bbdef.e_next,
		   txdata=[]}.

hashes(Block) when is_record(Block, bbdef) ->
	Block#bbdef{txdata=hashes(Block#bbdef.txdata)};
hashes(TxData) when is_list(TxData)->
	lists:map(fun(E) ->
	            #btxdef{txhash=binary:copy(E#btxdef.txhash),
	                txversion=E#btxdef.txversion,
	                inputcount=0,
	                outputcount=0,
	                txlocktime=E#btxdef.txlocktime,
	                 txinputs=[],
                     txoutputs=[]} end, TxData).

print(B) when is_record(B, bbdef) ->
    io:format("Hash ~p~nHeight~p~nPrevious:~p~nDifficulty:~p~n",
              [hex:bin_to_hexstr(hex:bin_reverse(B#bbdef.blockhash)),
               B#bbdef.e_height,
               hex:bin_to_hexstr(hex:bin_reverse(B#bbdef.previoushash)),
               B#bbdef.difficulty]).


serialize(B) when is_record(B, bbdef) ->
    Network= B#bbdef.network,
    HeaderLength = B#bbdef.headerlength,
    VersionNumber = B#bbdef.version,
    PreviousHash = B#bbdef.previoushash,
    MerkleRoot = B#bbdef.merkleroot,
    TimeStamp = B#bbdef.timestamp,
    TargetDifficulty = B#bbdef.difficulty,
    Nonce = B#bbdef.nonce,
    erlang:iolist_to_binary(
    [<<Network:32/little, 
    HeaderLength:32/little,
    VersionNumber:32/little, 
    PreviousHash:256/bitstring, 
    MerkleRoot:256/bitstring, 
    TimeStamp:32/little, 
    TargetDifficulty:32/little, 
    Nonce:32/little>>,
     lib_tx:int_to_varint(length(B#bbdef.txdata)),
     encode_txdata(B#bbdef.txdata)]).

encode_txdata(TxList) -> encode_txdata(TxList, []).
encode_txdata([], Acc) -> lists:reverse(Acc);
encode_txdata(TxList, Acc) ->
    [H|T] = TxList,
    encode_txdata(T, [lib_tx:serialize_btxdef(H)|Acc]).
