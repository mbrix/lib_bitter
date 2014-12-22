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

-export([hash160_to_address/1,
	     address_type/1,
	     p2sh_script_to_address/1,
	     p2sh_hash160_to_address/1,
		 address_to_hash160/1,
	     script_to_address/2,
	     info_addresses/2,
	     generate_keypair/0,
		 tx_to_hexstr/1,
	     private_to_wif/2,
	     wif_to_private/1,
	     compress_key/1,
	     key_to_hash160/1,
	     public_to_address/1,
	     public_to_hash160/1,
	     generate_p2sh_address/1,
	     p2sh_redeemscript/1,
		 p2sh_redeemscript_hash/1,
	     script_to_hash160/1,
	     verify_keypair/2,
	     openassets/1,
	     checksum/1,
	     checksum/2,
	     base58_check/2]).

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
	     readable/1,
	     issue_color/1]).

-include_lib("bitter.hrl").
-include_lib("eunit/include/eunit.hrl").

% Address normalization
% Address -> Hash160 should retain address type info

% Normalize script to addr record
new(Script) when is_binary(Script) ->
	Spec = lib_parse:parse_script(Script),
	{Type, _} = Spec,
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

new(Address) when is_record(Address, addr) ->
	Address.

new(p2pkh, Bin) when is_binary(Bin) ->
	#addr{type = p2pkh,
		  bin  = Bin};

new(p2sh, Bin) when is_binary(Bin) ->
	#addr{type = p2sh,
		  bin  = Bin};

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

hash160(Addr) ->
	Addr#addr.bin.

readable(Addr) when is_record(Addr, addr) ->
	case Addr#addr.type of
		p2pkh ->
			hash160_to_address(Addr#addr.bin);
		p2sh ->
			p2sh_hash160_to_address(Addr#addr.bin)
	end;

readable(Addr) when is_list(Addr) ->
    A = new(Addr),
	readable(A);

readable(Unspent) when is_record(Unspent, utxop) ->
    A = new(Unspent#utxop.script),
    readable(A).

openassets(Addr) when is_record(Addr, addr) ->
    case Addr#addr.type of
        p2pkh ->
            hash160_to_openassets(Addr#addr.bin);
        p2sh ->
            p2sh_hash160_to_openassets(Addr#addr.bin)
    end;

openassets(Addr) when is_list(Addr) ->
    A = new(Addr),
    openassets(A);

openassets(Unspent) when is_record(Unspent, utxop) ->
    A = new(Unspent#utxop.script),
    openassets(A).

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
    <<Type:8/bitstring, Rest/binary>> = base58:base58_to_binary(Address),
    address_hashtype(Type, Rest).

address_hashtype(<<0:8>>, BinAddress) ->
    <<K:160/bitstring, _/binary>> = BinAddress,
    {p2pkh, K};

address_hashtype(<<5:8>>, BinAddress) ->
    <<K:160/bitstring, _/binary>> = BinAddress,
    {p2sh, K};

address_hashtype(<<19:8>>, BinAddress) ->
    <<Type:8/bitstring, Rest/binary>> = BinAddress,
    address_hashtype(Type, Rest).

address_to_hash160(Address) ->
	<<_:8, K:160/bitstring, _/binary>> = base58:base58_to_binary(Address),
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

hash160_to_address(Script) when is_binary(Script) ->
	base58_check(<<0:8>>, Script).

p2sh_hash160_to_address(Script) when is_binary(Script) ->
	base58_check(<<5:8>>, Script).

hash160_to_openassets(Script) when is_binary(Script) ->
    base58_check([<<19:8>>, <<0:8>>], Script).

p2sh_hash160_to_openassets(Script) when is_binary(Script) ->
    base58_check([<<19:8>>, <<5:8>>], Script).

p2sh_script_to_address(Script) ->
	p2sh_hash160_to_address(script_to_hash160(Script)).

script_to_hash160(Script) ->
	crypto:hash(ripemd160, crypto:hash(sha256, Script)).

script_to_address({p2pkh, Pubkey}, _) -> Pubkey;
script_to_address({p2pkh2, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({full_pubkey, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({compressed_pubkey, Pubkey}, _) -> key_to_hash160(Pubkey);
script_to_address({malformed, _}, _) -> <<"malformed00000000000">>;
script_to_address({p2sh, Hash}, _) -> Hash;
script_to_address({multisig, {_, _}}, Script) -> script_to_hash160(Script);
script_to_address({op_return, _}, _) -> <<"op_return00000000000">>; 
script_to_address({openassets, _}, _) -> <<"openassets000000000">>;
script_to_address(_X, _) -> <<"weird000000000000000">>.


info_addresses({p2pkh, Pubkey}, _) -> [Pubkey];
info_addresses({p2pkh2, Pubkey}, _) -> [Pubkey];
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
	{UncompressedPublic, Private} = crypto:generate_key(ecdh, secp256k1),
	{UncompressedPublic, Private}.

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
public_to_address(Public) ->
	Hash160 = key_to_hash160(compress_key(Public)),
	hash160_to_address(Hash160).

public_to_hash160(Public) ->
	key_to_hash160(compress_key(Public)).

% Multisignature P2sh Addresses
p2sh_redeemscript_hash(PublicKeyList) ->
	script_to_hash160(p2sh_redeemscript(PublicKeyList)).

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

verify_keypair(Public, Private) ->
	Msg = crypto:rand_bytes(32),
	try
		Signature = crypto:sign(ecdsa, sha256, Msg, [Private, secp256k1]),
		crypto:verify(ecdsa, sha256, Msg, Signature, [Public, secp256k1])
	catch
		_:_ ->
			false
	end.
