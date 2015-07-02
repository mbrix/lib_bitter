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
% Consolidated Signing Logic for Validating Bitcoin Transactions
%
%

-module(lib_sign).
-author('mbranton@emberfinancial.com').
-include_lib("bitter.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([signing_hash/4,
		 canonical/1]).


clear_input(I) -> I#btxin{script = <<>>}.

zero_input(I) -> I#btxin{seqnum = 0}.
zero_output(O) -> O#btxout{script = <<>>,
						   value = -1}.

%% Canonical representation SIGHASH_ALL
canonical(Tx) ->
	Tx#btxdef{txinputs = lists:map(fun(I) -> clear_input(I) end, Tx#btxdef.txinputs)}.


signing_hash(CanonicalTx, InputIndex, SubScript, Signature) ->
	SigSize = size(Signature)-1,
	<<SigStr:(SigSize)/binary, SigHashType:8>> = Signature,
	<<_:(SigSize)/binary, _:3, SigType:5>> = Signature,
	case check_single(SigType, CanonicalTx, InputIndex) of
		error ->
			%% Concensus bug must return has of 1
			{SigStr, <<1:(256*8)/little>>};
		ok ->
			Tx2 = prepare_tx(SigType, CanonicalTx, InputIndex, SubScript),
			Tx3 = anyonecanpay(SigHashType band ?SIGHASH_ANYONECANPAY, Tx2, InputIndex),
			{SigStr,
			 lib_tx:hash_tx(iolist_to_binary([lib_tx:serialize_btxdef(Tx3), <<SigHashType:32/little>>]))}
	end.

anyonecanpay(0, Tx2, _) -> Tx2;
anyonecanpay(_, Tx2, Index) ->
	Tx2#btxdef{txinputs = [lists:nth(Index+1, Tx2#btxdef.txinputs)]}.

check_single(?SIGHASH_SINGLE, Tx, Index) ->
	if (Index+1) > length(Tx#btxdef.txoutputs) -> error;
	   true -> ok
	end;
check_single(_, _, _) -> ok.

prepare_tx(?SIGHASH_ALL, Tx, Index, SubScript) -> 
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txinputs = lists:map(fun(I) when I =:= InterestingInput ->
				  							   InterestingInput#btxin{script = SubScript};
			  						  (I2) -> I2
								   end, Tx#btxdef.txinputs)};

prepare_tx(?SIGHASH_NONE, Tx, Index, SubScript) ->
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txoutputs = [],
			  txinputs = lists:map(fun(I) when I =:= InterestingInput ->
				  							   InterestingInput#btxin{script = SubScript};
			  						  (I2) -> I2#btxin{seqnum = 0}
								   end, Tx#btxdef.txinputs)};

prepare_tx(?SIGHASH_SINGLE, Tx, Index, SubScript) ->
	SubList = lists:sublist(Tx#btxdef.txoutputs, 1, Index),
	CurrentOutput = lists:nth(Index+1, Tx#btxdef.txoutputs),
	InterestingInput = lists:nth(Index+1, Tx#btxdef.txinputs),
	Tx#btxdef{txoutputs = lists:map(fun(O) -> zero_output(O) end, SubList) ++ [CurrentOutput],
	  txinputs = lists:map(fun(I) when I =:= InterestingInput ->
				  							   InterestingInput#btxin{script = SubScript};
			  						  (I2) -> zero_input(I2)
								   end, Tx#btxdef.txinputs)};

prepare_tx(?SIGHASH_OLD, Tx, Index, SubScript) -> prepare_tx(?SIGHASH_ALL, Tx, Index, SubScript);
prepare_tx(_UnknownHash, Tx, Index, SubScript) -> prepare_tx(?SIGHASH_ALL, Tx, Index, SubScript).



