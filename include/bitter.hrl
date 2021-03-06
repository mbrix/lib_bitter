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
-author('mbranton@emberfinancial.com').
% Block Record Formats
-define(COINBASE, <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
-define(DUSTLIMIT, 600). %% 546
-define(DEFAULTFEE, 10000).
-define(FEE_PER_K, 1000).
-define(SIGHASH_OLD, 0).
-define(SIGHASH_ALL, 1).
-define(SIGHASH_NONE, 2).
-define(SIGHASH_SINGLE, 3).
-define(SIGHASH_ANYONECANPAY, 128).
-define(MISSINGSIG, <<255,255,255,255,255,255,255,255,255,255>>).

-define(MARKER, <<-1:256>>).

-define(WHERE, lib_notify:where(Pid, ?MODULE)).
-define(WHEREBLOCKD, lib_notify:where(Pid, bitter_blockd)).

-define(Unspent_Deleted, <<0:8>>).
-define(Unspent_Confirmed, <<1:8>>).
-define(Unspent_Unconfirmed, <<2:8>>).
-define(Spent_Unconfirmed, <<3:8>>).
-define(Spent_Confirmed, <<4:8>>).

-define(Uncolored, <<0:8>>).

% Always get the nearest cache
-define(WHERECACHE, lib_notify:where(near, bitter_objcache)).

% Unspent sets indexed by TX Hash

-record(unspentset, {type, mapping}).
-record(us, {hash_index, status, height_coinbase_output}).


% Raw block binary formats

-record(bblock, {data, ext=#{}, meta=#{}}).
-record(btx, {data, offset, ext=#{}, meta=#{}}).

-record(binput, {data, offset, ext, meta}).
-record(boutput, {data, offset, ext, meta}).


% Expanded to nclude cumulative fields
-record(bbdef, {network,
		        blockhash, 
		        headerlength, 
		        version, 
		        previoushash,
		        merkleroot,
		        timestamp,
		        difficulty=0,
		        nonce,
		        txcount,
		        txdata, 
		        e_sumdiff=0,
		        e_height=1,
		        e_next=undefined}).

-record(btxdef, {txhash,
		        txversion,
		        inputcount,
		        outputcount,
		        txlocktime,
		        txinputs,
				txoutputs}).

-record(btxin, {txhash, txindex, script = <<>>, seqnum, signed=false}).
-record(btxout, {txindex, value, script = <<>>, address="", info, attributes=#{}}).

%% Extended transaction information

-record(btxdef_ext, {tx, info}).

% Magic numbers
-define(MAGICBYTE_LIVE, 16#D9B4BEF9).
-define(MAGICBYTE_TESTNET, 16#DAB5BFFA).
-define(MAGICBYTE_TESTNET3, 16#0709110B).
-define(MAGICBYTE_NAMECOIN, 16#F9BEB4FE).

% Root of Chain
-define(CHAIN_ROOT, <<0:256>>).

% Chain Definitions
-record(bchaindef, {chainid,
		            chain,
		            sumdifficulty,
		            blockheight,
					qlen}).

% Unspent Transaction Pool
-record(utxop, {hash_index, value, script, address, info, attributes=#{}, height, state, coinbase}).

% Address mapping pool
-record(address, {address, hash_index}).

% Address Format
-record(addr, {type, bin}).

% Color Format
-record(color, {bin, name, asset_ids, contract_url, short_name, issuer, description, mime_type, type, divisibility, link_to_website, icon_url, image_url, version, verified=false}).


% Config information
-record(config, {module_name, state}).
-record(utxo_state, {last_seen=undefined, height=0, last_block}).

% Keyserver (KSM) records
%%-record(keystorage, {address, public_key, private_key}).
-define(KEYSTORAGE_VERSION, 1).
-record(keystorage, {id, version, key}).

% Address records for P2SH and other address construction

-record(addr_registry, {address, type, keylist}).

% Payee Records for generating arbitrary transactions
% Change is the change address for a transaction
% In the event of a color overflow or BTC remainder

-record(payee, {address, change, color, value}).

% Payment record
% contains selected unspents
% Remaning change and color
% Payment records are transformed into transactions
% change is the last specified change address

-record(payment, {selected=[], outputs=[], change,
                  r_color=?Uncolored, r_value=0,
                  issuances=0, fee=0, metaurl=undefined}).

% txpool for tracking transactions submitted to the network
-record(txpool, {txhash, timestamp, tx, status}).

% Inventory Vector Types
-define(INV_ERROR, <<0:32/little>>).
-define(INV_TX, <<1:32/little>>).
-define(INV_BLOCK, <<2:32/little>>).

% Block Sequence / Concurrent slow consumers
-record(bitter_seq, {num, direction, blockhash}).

% Regular key formats
-record(p2pkh, {private}).

% HD key format

-record(bip32_priv_key, {key, chain_code, depth, child_num, finger_print, network}).
-record(bip32_pub_key, {key, chain_code, depth, child_num, finger_print, network}).

-record(bip45_key, {private, cosigner_keys, locked}).


% Network specific structures
-record(rejectmsg, {msg, ccode, reason, extra}).

% Script OP Codes
% push values
-define(OP_0, 16#00). %0
-define(OP_FALSE, ?OP_0).
-define(OP_PUSHDATA1, 16#4c). %76
-define(OP_PUSHDATA2, 16#4d). %77
-define(OP_PUSHDATA4, 16#4e). %78
-define(OP_1NEGATE, 16#4f). %79
-define(OP_RESERVED, 16#50). %80
-define(OP_1, 16#51). %81
-define(OP_TRUE, ?OP_1).
-define(OP_2, 16#52). %82
-define(OP_3, 16#53). %83
-define(OP_4, 16#54). %84
-define(OP_5, 16#55). %85
-define(OP_6, 16#56). %86
-define(OP_7, 16#57). %87
-define(OP_8, 16#58). %88
-define(OP_9, 16#59). %89
-define(OP_10, 16#5a). %90
-define(OP_11, 16#5b). %91
-define(OP_12, 16#5c). %92
-define(OP_13, 16#5d). %93
-define(OP_14, 16#5e). %94
-define(OP_15, 16#5f). %95
-define(OP_16, 16#60). %96
% control.
-define(OP_NOP, 16#61). %97
-define(OP_VER, 16#62). %98
-define(OP_IF, 16#63). %99
-define(OP_NOTIF, 16#64). %100
-define(OP_VERIF, 16#65). %101
-define(OP_VERNOTIF, 16#66). %102
-define(OP_ELSE, 16#67). %103
-define(OP_ENDIF, 16#68). %104
-define(OP_VERIFY, 16#69). %105
-define(OP_RETURN, 16#6a). %106
% stack ops
-define(OP_TOALTSTACK, 16#6b). %107
-define(OP_FROMALTSTACK, 16#6c). %108
-define(OP_2DROP, 16#6d). %109
-define(OP_2DUP, 16#6e). %110
-define(OP_3DUP, 16#6f). %111
-define(OP_2OVER, 16#70). %112
-define(OP_2ROT, 16#71). %113
-define(OP_2SWAP, 16#72). %114
-define(OP_IFDUP, 16#73). %115
-define(OP_DEPTH, 16#74). %116
-define(OP_DROP, 16#75). %117
-define(OP_DUP, 16#76). %118
-define(OP_NIP, 16#77). %119
-define(OP_OVER, 16#78). %120
-define(OP_PICK, 16#79). %121
-define(OP_ROLL, 16#7a). %122
-define(OP_ROT, 16#7b). %123
-define(OP_SWAP, 16#7c). %124
-define(OP_TUCK, 16#7d). %125
% splice ops
-define(OP_CAT, 16#7e). %126
-define(OP_SUBSTR, 16#7f). %127
-define(OP_LEFT, 16#80). %128
-define(OP_RIGHT, 16#81). %129
-define(OP_SIZE, 16#82). %130
% bit logic
-define(OP_INVERT, 16#83). %131
-define(OP_AND, 16#84). %132
-define(OP_OR, 16#85). %133
-define(OP_XOR, 16#86). %134
-define(OP_EQUAL, 16#87). %135
-define(OP_EQUALVERIFY, 16#88). %136
-define(OP_RESERVED1, 16#89). %137
-define(OP_RESERVED2, 16#8a). %138
% numeric.
-define(OP_1ADD, 16#8b). %139
-define(OP_1SUB, 16#8c). %140
-define(OP_2MUL, 16#8d). %141
-define(OP_2DIV, 16#8e). %142
-define(OP_NEGATE, 16#8f). %143
-define(OP_ABS, 16#90). %144
-define(OP_NOT, 16#91). %145
-define(OP_0NOTEQUAL, 16#92). %146
-define(OP_ADD, 16#93). %147
-define(OP_SUB, 16#94). %148
-define(OP_MUL, 16#95). %149
-define(OP_DIV, 16#96). %150
-define(OP_MOD, 16#97). %151
-define(OP_LSHIFT, 16#98). %152
-define(OP_RSHIFT, 16#99). %153
-define(OP_BOOLAND, 16#9a). %154
-define(OP_BOOLOR, 16#9b). %155
-define(OP_NUMEQUAL, 16#9c). %156
-define(OP_NUMEQUALVERIFY, 16#9d). %157
-define(OP_NUMNOTEQUAL, 16#9e). %158
-define(OP_LESSTHAN, 16#9f). %159
-define(OP_GREATERTHAN, 16#a0). %160
-define(OP_LESSTHANOREQUAL, 16#a1). %161
-define(OP_GREATERTHANOREQUAL, 16#a2). %162
-define(OP_MIN, 16#a3). %163
-define(OP_MAX, 16#a4). %164
-define(OP_WITHIN, 16#a5). %165
% crypto
-define(OP_RIPEMD160, 16#a6). %166
-define(OP_SHA1, 16#a7). %167
-define(OP_SHA256, 16#a8). %168
-define(OP_HASH160, 16#a9). %169
-define(OP_HASH256, 16#aa). %170
-define(OP_CODESEPARATOR, 16#ab). %171
-define(OP_CHECKSIG, 16#ac). %172
-define(OP_CHECKSIGVERIFY, 16#ad). %173
-define(OP_CHECKMULTISIG, 16#ae). %174
-define(OP_CHECKMULTISIGVERIFY, 16#af). %175
% expansion.
-define(OP_NOP1, 16#b0). %176
-define(OP_NOP2, 16#b1). %177
-define(OP_NOP3, 16#b2). %178
-define(OP_NOP4, 16#b3). %179
-define(OP_NOP5, 16#b4). %180
-define(OP_NOP6, 16#b5). %181
-define(OP_NOP7, 16#b6). %182
-define(OP_NOP8, 16#b7). %183
-define(OP_NOP9, 16#b8). %184
-define(OP_NOP10, 16#b9). %185
% template matching params.
-define(OP_SMALLDATA, 16#f9). %249
-define(OP_SMALLINTEGER, 16#fa). %250
-define(OP_PUBKEYS, 16#fb). %251
-define(OP_PUBKEYHASH, 16#fd). %253
-define(OP_PUBKEY, 16#fe). %254
-define(OP_INVALIDOPCODE, 16#ff). %255

%% Let's wrap up all of the op codes into an operation map of ops to atoms


-define(OP_MAP, #{?OP_0 => op_0,
				  ?OP_PUSHDATA1 => op_pushdata1,
				  ?OP_PUSHDATA2 => op_pushdata2,
				  ?OP_PUSHDATA4 => op_pushdata4,
				  ?OP_1NEGATE => op_1negate,
				  ?OP_RESERVED => op_reserved,
				  ?OP_1 => op_1,
				  ?OP_TRUE => op_1,
				  ?OP_2 => op_2,
				  ?OP_3 => op_3,
				  ?OP_4 => op_4,
				  ?OP_5 => op_5,
				  ?OP_6 => op_6,
				  ?OP_7 => op_7,
				  ?OP_8 => op_8,
				  ?OP_9 => op_9,
				  ?OP_10 => op_10,
				  ?OP_11 => op_11,
				  ?OP_12 => op_12,
				  ?OP_13 => op_13,
				  ?OP_14 => op_14,
				  ?OP_15 => op_15,
				  ?OP_16 => op_16,
				  ?OP_NOP => op_nop,
				  ?OP_VER => op_ver,
				  ?OP_IF => op_if,
				  ?OP_NOTIF => op_notif,
				  ?OP_VERIF => op_verif,
				  ?OP_VERNOTIF => op_vernotif,
				  ?OP_ELSE => op_else,
				  ?OP_ENDIF => op_endif,
				  ?OP_VERIFY => op_verify,
				  ?OP_RETURN => op_return,
				  ?OP_TOALTSTACK => op_toaltstack,
				  ?OP_FROMALTSTACK => op_fromaltstack,
				  ?OP_2DROP => op_2drop,
				  ?OP_2DUP => op_2dup,
				  ?OP_3DUP => op_3dup,
				  ?OP_2OVER => op_2over,
				  ?OP_2ROT => op_2rot,
				  ?OP_2SWAP => op_2swap,
				  ?OP_IFDUP => op_ifdup,
				  ?OP_DEPTH => op_depth,
				  ?OP_DROP => op_drop,
				  ?OP_DUP => op_dup,
				  ?OP_NIP => op_nip,
				  ?OP_OVER => op_over,
				  ?OP_PICK => op_pick,
				  ?OP_ROLL => op_roll,
				  ?OP_ROT => op_rot,
				  ?OP_SWAP => op_swap,
				  ?OP_TUCK => op_tuck,
				  ?OP_CAT => op_cat,
				  ?OP_SUBSTR => op_substr,
				  ?OP_LEFT => op_left,
				  ?OP_RIGHT => op_right,
				  ?OP_SIZE => op_size,
				  ?OP_INVERT => op_invert,
				  ?OP_AND => op_and,
				  ?OP_OR => op_or,
				  ?OP_XOR => op_xor,
				  ?OP_EQUAL => op_equal,
				  ?OP_EQUALVERIFY => op_equalverify,
				  ?OP_RESERVED1 => op_reserved1,
				  ?OP_RESERVED2 => op_reserved2,
				  ?OP_1ADD => op_1add,
				  ?OP_1SUB => op_1sub,
				  ?OP_2MUL => op_2mul,
				  ?OP_2DIV => op_2div,
				  ?OP_NEGATE => op_negate,
				  ?OP_ABS => op_abs,
				  ?OP_NOT => op_not,
				  ?OP_0NOTEQUAL => op_0notequal,
				  ?OP_ADD => op_add,
				  ?OP_SUB => op_sub,
				  ?OP_MUL => op_mul,
				  ?OP_DIV => op_div,
				  ?OP_MOD => op_mod,
				  ?OP_LSHIFT => op_lshift,
				  ?OP_RSHIFT => op_rshift,
				  ?OP_BOOLAND => op_booland,
				  ?OP_BOOLOR => op_boolor,
				  ?OP_NUMEQUAL => op_numequal,
				  ?OP_NUMEQUALVERIFY => op_numequalverify,
				  ?OP_NUMNOTEQUAL => op_numnotequal,
				  ?OP_LESSTHAN => op_lessthan,
				  ?OP_GREATERTHAN => op_greaterthan,
				  ?OP_LESSTHANOREQUAL => op_lessthanorequal,
				  ?OP_GREATERTHANOREQUAL => op_greaterthanorequal,
				  ?OP_MIN => op_min,
				  ?OP_MAX => op_max,
				  ?OP_WITHIN => op_within,
				  ?OP_RIPEMD160 => op_ripemd160,
				  ?OP_SHA1 => op_sha1,
				  ?OP_SHA256 => op_sha256,
				  ?OP_HASH160 => op_hash160,
				  ?OP_HASH256 => op_hash256,
				  ?OP_CODESEPARATOR => op_codeseparator,
				  ?OP_CHECKSIG => op_checksig,
				  ?OP_CHECKSIGVERIFY => op_checksigverify,
				  ?OP_CHECKMULTISIG => op_checkmultisig,
				  ?OP_CHECKMULTISIGVERIFY => op_checkmultisigverify,
				  ?OP_NOP1 => op_nop1,
				  ?OP_NOP2 => op_nop2,
				  ?OP_NOP3 => op_nop3,
				  ?OP_NOP4 => op_nop4,
				  ?OP_NOP5 => op_nop5,
				  ?OP_NOP6 => op_nop6,
				  ?OP_NOP7 => op_nop7,
				  ?OP_NOP8 => op_nop8,
				  ?OP_NOP9 => op_nop9,
				  ?OP_NOP10 => op_nop10,
				  ?OP_SMALLDATA => op_smalldata,
				  ?OP_SMALLINTEGER => op_smallinteger,
				  ?OP_PUBKEYS => op_pubkeys,
				  ?OP_PUBKEYHASH => op_pubkeyhash,
				  ?OP_PUBKEY => op_pubkey,
				  ?OP_INVALIDOPCODE => op_invalidopcode}).
