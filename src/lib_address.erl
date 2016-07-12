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

-module(lib_address).
-author('mbranton@emberfinancial.com').

-export([hash160_to_address/2,
	     address_type/1,
	     p2sh_script_to_address/2,
	     p2sh_hash160_to_address/2,
		 address_to_hash160/1,
	     script_to_address/2,
	     info_addresses/2,
	     generate_keypair/0,
		 tx_to_hexstr/1,
	     private_to_wif/2,
	     wif_to_private/1,
	     compress_key/1,
	     key_to_hash160/1,
	     public_to_address/2,
	     public_to_hash160/1,
	     generate_p2sh_address/1,
	     p2sh_redeemscript/1,
		 p2sh_redeemscript_hash/1,
	     script_to_hash160/1,
	     verify_keypair/2,
	     openassets/2,
	     openassets/3,
	     is_openassets/2,
	     checksum/1,
	     checksum/2,
	     base58_check/2,
	     private/1,
	     public/1]).

-export([new/1,
         new/2,
         new/3,
         new/4,
	     equal/2,
	     equal/3,
	     equal/4,
	     type/1,
	     hash160/1,
	     script/1,
	     readable/2,
	     readable/3,
	     issue_color/1]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

% Address normalization
% Address -> Hash160 should retain address type info

% Normalize script to addr record
new(Script) when is_binary(Script) ->
	Spec = lib_parse:parse_script(Script),
    Type = case Spec of
               {T, _} -> T;
               {T, _, _}  -> T
           end,
	BinaryAddress = script_to_address(Spec, Script),
	#addr{ type = Type,
		   bin = BinaryAddress};

new(Address) when is_list(Address) ->
    try
        case checksum(Address) of
            true ->
                {Type, Bin} = address_hashtype(Address),
                #addr{type = Type,
                      bin = Bin};
            false ->
                throw(address_error)
        end
    catch
        _:_ -> throw(address_error)
    end;

new(Output) when is_record(Output, btxout) ->
	new(Output#btxout.script);

new(Unspent) when is_record(Unspent, utxop) ->
	new(Unspent#utxop.script);

new(Address) when is_record(Address, addr) ->
	Address.

new(binstr, Address) ->
	new(erlang:binary_to_list(Address));

new(p2pkh, Bin) when is_binary(Bin) ->
	#addr{type = p2pkh,
		  bin  = Bin};

new(p2sh, Bin) when is_binary(Bin) ->
	#addr{type = p2sh,
		  bin  = Bin};

new(key, KeyList) when is_list(KeyList) ->
	RedeemScript = p2sh_redeemscript(KeyList),
	#addr{type = p2sh,
		  bin = script_to_hash160(RedeemScript)};

new(key, PublicKey) ->
	#addr{ type = p2pkh,
		   bin  = key_to_hash160(compress_key(PublicKey))}.

new(key, PublicKey, PublicKey2) ->
	RedeemScript = p2sh_redeemscript([PublicKey, PublicKey2]),
	#addr{type = p2sh,
		  bin = script_to_hash160(RedeemScript)}.

new(key, PublicKey, PublicKey2, PublicKey3) ->
	RedeemScript = p2sh_redeemscript([PublicKey, PublicKey2, PublicKey3]),
	#addr{type = p2sh,
		  bin = script_to_hash160(RedeemScript)}.


% Does the Public key hash to this address
equal(Address, Address2) when is_record(Address, addr), is_record(Address2, addr) ->
	Address =:= Address2;

equal(Address, PublicKey) ->
	A = new(key, PublicKey),
	Address =:= A.

equal(Address, PublicKey, PublicKey2) ->
	A = new(key, PublicKey, PublicKey2),
	Address =:= A.

equal(Address, PublicKey, PublicKey2, PublicKey3) ->
	A = new(key, PublicKey, PublicKey2, PublicKey3),
	Address =:= A.

type(Addr) ->
	Addr#addr.type.

hash160(Addr) -> Addr#addr.bin.

string_to_bin(S) when is_atom(S) -> S;
string_to_bin(S) when is_list(S) -> erlang:list_to_binary(S);
string_to_bin(S) -> S.

readable(binary, NetworkParams, Addr) ->
	string_to_bin(readable(NetworkParams, Addr)).

readable(NetworkParams, Addr) when is_record(Addr, addr) ->
	try
		case Addr#addr.type of
			p2sh -> p2sh_hash160_to_address(NetworkParams, Addr#addr.bin);
			_ -> hash160_to_address(NetworkParams, Addr#addr.bin)
		end
	catch _:_ -> "unsupported"
	end;

readable(NetworkParams, Addr) when is_list(Addr) ->
    A = new(Addr),
	readable(NetworkParams, A);

readable(NetworkParams, Unspent) when is_record(Unspent, utxop) ->
    A = new(Unspent#utxop.script),
    readable(NetworkParams, A);

readable(NetworkParams, Output) when is_record(Output, btxout) ->
	A = new(Output#btxout.script),
	readable(NetworkParams, A).

openassets(binary, NetworkParams, Addr) ->
	string_to_bin(openassets(NetworkParams, Addr)).

openassets(NetworkParams, Addr) when is_record(Addr, addr) ->
    case Addr#addr.type of
        p2pkh ->
            hash160_to_openassets(NetworkParams, Addr#addr.bin);
        p2sh ->
            p2sh_hash160_to_openassets(NetworkParams, Addr#addr.bin)
    end;

openassets(NetworkParams, Addr) when is_list(Addr) ->
    A = new(Addr),
    openassets(NetworkParams, A);

openassets(NetworkParams, Unspent) when is_record(Unspent, utxop) ->
    A = new(Unspent#utxop.script),
    openassets(NetworkParams, A).

is_openassets(NetworkParams, Address) when is_list(Address) ->
	try
		A = lib_address:new(Address),
		openassets(NetworkParams, A) =:= Address
	catch
		throw:address_error -> false
	end.

script(Addr) when is_record(Addr, addr) ->
	create_script(Addr#addr.type, Addr#addr.bin).

create_script(p2pkh, Bin) ->
	<<?OP_DUP:8, ?OP_HASH160:8, 16#14:8, Bin:160/bitstring, ?OP_EQUALVERIFY:8, ?OP_CHECKSIG:8>>;
create_script(p2sh, Bin) ->
	<<?OP_HASH160:8, 16#14:8, Bin:160/bitstring, ?OP_EQUAL:8>>.

issue_color(Addr) when is_record(Addr, addr) ->
    lib_color:new(lib_color:script_to_ic(script(Addr))).


% Address is a raw 20 bit hash160 address

address_type(L) when is_list(L) ->
	address_type(erlang:list_to_binary(L));
address_type(<<$1:8, _/binary>>) -> p2pkh;
address_type(<<$3:8, _/binary>>) -> p2sh.

address_hashtype(Address) ->
    <<Type:1/binary, Rest/binary>> = base58:base58_to_binary(Address),
    address_hashtype(Type, Rest).

%% This is hardcoded in to detect either main or testnet3 need a much nicer way
%% to do this.

address_hashtype(<<0:8>>, BinAddress) ->
    <<K:20/binary, _/binary>> = BinAddress,
    {p2pkh, K};

address_hashtype(<<5:8>>, BinAddress) ->
    <<K:20/binary, _/binary>> = BinAddress,
    {p2sh, K};

address_hashtype(<<19:8>>, BinAddress) ->
    <<Type:1/binary, Rest/binary>> = BinAddress,
    address_hashtype(Type, Rest);

address_hashtype(<<111:8>>, BinAddress) ->
    <<K:20/binary, _/binary>> = BinAddress,
    {p2pkh, K};

address_hashtype(<<196:8>>, BinAddress) ->
    <<K:20/binary, _/binary>> = BinAddress,
    {p2sh, K}.

address_to_hash160(Address) ->
	<<_:8, K:20/binary, _/binary>> = base58:base58_to_binary(Address),
	K.

base58_check(V, Script) ->
	<<C:32/bitstring,_/binary>> = crypto:hash(sha256,
			crypto:hash(sha256, iolist_to_binary([V, Script]))), 
	A = iolist_to_binary([V, Script, C]),
	base58:binary_to_base58(A).

checksum(LeadingByte, Address) ->
    try
        <<LeadingByte:8, _Rest/binary>> = base58:base58_to_binary(Address),
        checksum(Address)
    catch
        _:_ -> false
    end.

checksum(Address) ->
    <<ChecksumR:32/bitstring, R/binary>> = hex:bin_reverse(base58:base58_to_binary(Address)),
    <<Checksum:32/bitstring, _/binary>> = crypto:hash(sha256, crypto:hash(sha256, hex:bin_reverse(R))),
    Checksum =:= hex:bin_reverse(ChecksumR).

hash160_to_address(NetworkParams, Script) when is_binary(Script) ->
	base58_check(maps:get(p2pkh_checkbyte, NetworkParams), Script).

p2sh_hash160_to_address(NetworkParams, Script) when is_binary(Script) ->
	base58_check(maps:get(p2sh_checkbyte, NetworkParams), Script).

hash160_to_openassets(NetworkParams, Script) when is_binary(Script) ->
    base58_check([maps:get(oa_checkbyte, NetworkParams), <<0:8>>], Script).

p2sh_hash160_to_openassets(NetworkParams, Script) when is_binary(Script) ->
    base58_check([maps:get(oa_checkbyte, NetworkParams), <<5:8>>], Script).

p2sh_script_to_address(NetworkParams, Script) ->
	p2sh_hash160_to_address(NetworkParams, script_to_hash160(Script)).

script_to_hash160(Script) ->
	crypto:hash(ripemd160, crypto:hash(sha256, Script)).

script_to_address({p2pkh, Pubkey}, _) -> Pubkey;
script_to_address({p2pkh2, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({full_pubkey, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({compressed_pubkey, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({malformed, _}, _) -> <<"malformed00000000000">>;
script_to_address({p2sh, _Subtype, Hash}, _) -> Hash;
script_to_address({multisig, {_, _}}, Script) -> script_to_hash160(Script);
script_to_address({op_return, _}, _) -> <<"op_return00000000000">>; 
script_to_address({openassets, _}, _) -> <<"openassets000000000">>;
script_to_address(_X, _) -> <<"weird000000000000000">>.


info_addresses({p2pkh, Pubkey}, _) -> [Pubkey];
info_addresses({p2pkh2, Pubkey}, _) -> [Pubkey];
info_addresses({p2sh, _Subtype, Pubkey}, _) -> [Pubkey];
info_addresses({full_pubkey, Pubkey}, _) -> [Pubkey];
info_addresses({compressed_pubkey, Pubkey}, _) -> [Pubkey];
info_addresses({malformed, _}, _) -> [];
info_addresses({p2sh, {_, PubkeyList}}, Script) -> 
	[lib_address:p2sh_script_to_address(Script)|PubkeyList];
info_addresses({op_return, _}, _) -> [];
info_addresses({openassets, _}, _) -> [];
info_addresses(_X, _) -> []. %got strange decoding

compress_key(<<4:8, A:256/bitstring, B:256>>) ->
	case B rem 2 of
		0 ->
			iolist_to_binary([<<2:8>>,A]);
		1 ->
			iolist_to_binary([<<3:8>>,A])
	end;
compress_key(<<2:8, R/binary>>) ->
	erlang:iolist_to_binary([<<2:8>>, R]);
compress_key(<<3:8, R/binary>>) ->
	erlang:iolist_to_binary([<<3:8>>, R]).



key_to_hash160(Pubkey) ->
	crypto:hash(ripemd160, crypto:hash(sha256, Pubkey)).

generate_keypair() ->
	Private = crypto:strong_rand_bytes(32),
	case libsecp256k1:ec_pubkey_create(Private, compressed) of
		{ok, CompressedPublic} -> {CompressedPublic, Private};
		error -> generate_keypair() %% If points are invalid
	end.

tx_to_hexstr(Txhash) ->
	hex:bin_to_hexstr(hex:bin_reverse(Txhash)).

private_to_wif(uncompressed, PrivateKey) ->
	%CompressedPublic = erlang:iolist_to_binary([PrivateKey, <<1:8>>]),
	base58_check(<<16#80:8>>, PrivateKey);
private_to_wif(compressed, PrivateKey) ->
	CompressedPublic = erlang:iolist_to_binary([PrivateKey, <<1:8>>]),
	base58_check(<<16#80:8>>, CompressedPublic).


wif_to_private(PrivateKey) ->
	case string:substr(PrivateKey, 1, 1) of
		"L" ->
			wif_to_private(compressed, PrivateKey);
		"K" ->
			wif_to_private(compressed, PrivateKey);
		_ ->
			wif_to_private(uncompressed, PrivateKey)
	end.

wif_to_private(compressed, PrivateKey) ->
	RawPrivate = base58:base58_to_binary(PrivateKey),
	PrivateSize = size(RawPrivate)*8-6*8,
	<<_:8, K:PrivateSize/bitstring, _/binary>> = base58:base58_to_binary(PrivateKey),
	K;

wif_to_private(uncompressed, PrivateKey) ->
	RawPrivate = base58:base58_to_binary(PrivateKey),
	PrivateSize = size(RawPrivate)*8-5*8,
	<<_:8, K:PrivateSize/bitstring, _/binary>> = base58:base58_to_binary(PrivateKey),
	K.


% Generated Compressed Public Key Addresses
public_to_address(NetworkParams, Public) ->
	Hash160 = key_to_hash160(compress_key(Public)),
	hash160_to_address(NetworkParams, Hash160).

public_to_hash160(Public) ->
	key_to_hash160(compress_key(Public)).

% Multisignature P2sh Addresses
p2sh_redeemscript_hash(PublicKeyList) ->
	script_to_hash160(p2sh_redeemscript(PublicKeyList)).

p2sh_redeemscript([Pubkey1]) ->
	CPubkey1 = compress_key(Pubkey1),
	<<?OP_1:8, 33:8, CPubkey1:264/bitstring, ?OP_1:8, ?OP_CHECKMULTISIG:8>>;

p2sh_redeemscript([Pubkey1, Pubkey2]) ->
	CPubkey1 = compress_key(Pubkey1),
	CPubkey2 = compress_key(Pubkey2),
	<<?OP_2:8, 33:8, CPubkey1:264/bitstring, 33:8, CPubkey2:264/bitstring, ?OP_2:8, ?OP_CHECKMULTISIG:8>>;

p2sh_redeemscript([Pubkey1, Pubkey2, Pubkey3]) ->
	CPubkey1 = compress_key(Pubkey1),
	CPubkey2 = compress_key(Pubkey2),
	CPubkey3 = compress_key(Pubkey3),
	<<?OP_2:8, 33:8, CPubkey1:264/bitstring, 33:8, CPubkey2:264/bitstring, 33:8, CPubkey3:264/bitstring, ?OP_3:8, ?OP_CHECKMULTISIG:8>>.

generate_p2sh_address(p2sh_2of2) ->
	{K1public, K1private} = generate_keypair(),
	{K2public, K2private} = generate_keypair(),
	{new(key, K1public, K2public), [{K1public, K1private},
			{K2public, K2private}]};

generate_p2sh_address(p2sh_2of3) ->
	{K1public, K1private} = generate_keypair(),
	{K2public, K2private} = generate_keypair(),
	{K3public, K3private} = generate_keypair(),
	{new(key, K1public, K2public, K3public),
		 [{K1public, K1private},
		  {K2public, K2private},
		  {K3public, K3private}]}.

verify_keypair(Public, Key) when is_record(Key, bip32_priv_key) ->
	verify_keypair(Public, private(Key));
verify_keypair(Public, Key) when is_record(Key, bip32_pub_key) ->
	Public =:= public(Key);
verify_keypair(Public, Key) when is_record(Key, bip45_key) ->
	Public =:= public(Key);
verify_keypair(Public, Private) ->
	Msg = crypto:strong_rand_bytes(32),
	try
		{ok, Signature} = libsecp256k1:ecdsa_sign(Msg, Private, default, <<>>),
		ok = libsecp256k1:ecdsa_verify(Msg, Signature, Public),
		true
	catch
		_:_ ->
			false
	end.


private(#p2pkh{private = Private}) -> Private;
private(Key) -> lib_hd:private(Key).

public(#p2pkh{private = Private}) ->
	{ok, Pubkey} = libsecp256k1:ec_pubkey_create(Private, compressed),
	Pubkey;
public(Key) when is_record(Key, bip45_key) -> lib_hdpm:public(Key);
public(Key) when is_record(Key, bip32_priv_key) -> lib_hd:public(Key);
public(Key) when is_record(Key, bip32_pub_key) -> lib_hd:public(Key).

