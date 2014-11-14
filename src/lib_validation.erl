
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

%% Bitter is not fully validating and should be fronted by
%% a validating node. However, certain validations are useful to ensure
%% proper unspent selection and spending.
%%
%% Collection of validation routines that match
%% https://en.bitcoin.it/wiki/Protocol_rules
%%
%% and explanations where certain validations happen in the code

-module(lib_validation).
-author('mbranton@emberfinancial.com').


%% TX Validation
%%

%%  Check syntactic correctness
%%  Make sure neither in or out lists are empty
%%  Size in bytes < MAX_BLOCK_SIZE
%%  Each output value, as well as the total, must be in legal money range
%%  Make sure none of the inputs have hash=0, n=-1 (coinbase transactions)
%%  Check that nLockTime <= INT_MAX[1], size in bytes >= 100[2], and sig opcount <= 2[3]
%%  Reject "nonstandard" transactions: scriptSig doing anything other than pushing numbers on the stack, or scriptPubkey not matching the two usual forms[4]
%%  Reject if we already have matching tx in the pool, or in a block in the main branch
%%  For each input, if the referenced output exists in any other tx in the pool, reject this transaction.[5]
%%  For each input, look in the main branch and the transaction pool to find the referenced output transaction. If the output transaction is missing for any input, this will be an orphan transaction. Add to the orphan transactions, if a matching transaction is not in there already.
%%  For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject this transaction
%%  For each input, if the referenced output does not exist (e.g. never existed or has already been spent), reject this transaction[6]
%%  Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range
%%  Reject if the sum of input values < sum of output values
%%  Reject if transaction fee (defined as sum of input values minus sum of output values) would be too low to get into an empty block
%%  Verify the scriptPubKey accepts for each input; reject if any are bad
%%  Add to transaction pool[7]
%%  "Add to wallet if mine"
%%  Relay transaction to peers
%%  For each orphan transaction that uses this one as one of its inputs, run all these steps (including this one) recursively on that orphan
%%  

%%  
%% Block Validation
%%

%% These messages hold a single block.
%% Check syntactic correctness
%% Reject if duplicate of block we have in any of the three categories
%% Transaction list must be non-empty
%% Block hash must satisfy claimed nBits proof of work
%% Block timestamp must not be more than two hours in the future
%% First transaction must be coinbase (i.e. only 1 input, with hash=0, n=-1), the rest must not be
%% For each transaction, apply "tx" checks 2-4
%% For the coinbase (first) transaction, scriptSig length must be 2-100
%% Reject if sum of transaction sig opcounts > MAX_BLOCK_SIGOPS
%% Verify Merkle hash
%% Check if prev block (matching prev hash) is in main branch or side branches. If not, add this to orphan blocks, then query peer we got this from for 1st missing orphan block in prev chain; done with block
%% Check that nBits value matches the difficulty rules
%% Reject if timestamp is the median time of the last 11 blocks or before
%% For certain old blocks (i.e. on initial block download) check that hash matches known values
%% Add block into the tree. There are three cases: 1. block further extends the main branch; 2. block extends a side branch but does not add enough difficulty to make it become the new main branch; 3. block extends a side branch and makes it the new main branch.
%% For case 1, adding to main branch:
%% For all but the coinbase transaction, apply the following:
%% For each input, look in the main branch to find the referenced output transaction. Reject if the output transaction is missing for any input.
%% For each input, if we are using the nth output of the earlier transaction, but it has fewer than n+1 outputs, reject.
%% For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject.
%% Verify crypto signatures for each input; reject if any are bad
%% For each input, if the referenced output has already been spent by a transaction in the main branch, reject
%% Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range
%% Reject if the sum of input values < sum of output values
%% Reject if coinbase value > sum of block creation fee and transaction fees
%% (If we have not rejected):
%% For each transaction, "Add to wallet if mine"
%% For each transaction in the block, delete any matching transaction from the transaction pool
%% Relay block to our peers
%% If we rejected, the block is not counted as part of the main branch
%% For case 2, adding to a side branch, we don't do anything.
%% For case 3, a side branch becoming the main branch:
%% Find the fork block on the main branch which this side branch forks off of
%% Redefine the main branch to only go up to this fork block
%% For each block on the side branch, from the child of the fork block to the leaf, add to the main branch:
%% Do "branch" checks 3-11
%% For all but the coinbase transaction, apply the following:
%% For each input, look in the main branch to find the referenced output transaction. Reject if the output transaction is missing for any input.
%% For each input, if we are using the nth output of the earlier transaction, but it has fewer than n+1 outputs, reject.
%% For each input, if the referenced output transaction is coinbase (i.e. only 1 input, with hash=0, n=-1), it must have at least COINBASE_MATURITY (100) confirmations; else reject.
%% Verify crypto signatures for each input; reject if any are bad
%% For each input, if the referenced output has already been spent by a transaction in the main branch, reject
%% Using the referenced output transactions to get input values, check that each input value, as well as the sum, are in legal money range
%% Reject if the sum of input values < sum of output values
%% Reject if coinbase value > sum of block creation fee and transaction fees
%% (If we have not rejected):
%% For each transaction, "Add to wallet if mine"
%% If we reject at any point, leave the main branch as what it was originally, done with block
%% For each block in the old main branch, from the leaf down to the child of the fork block:
%% For each non-coinbase transaction in the block:
%% Apply "tx" checks 2-9, except in step 8, only look in the transaction pool for duplicates, not the main branch
%% Add to transaction pool if accepted, else go on to next transaction
%% For each block in the new main branch, from the child of the fork node to the leaf:
%% For each transaction in the block, delete any matching transaction from the transaction pool
%% Relay block to our peers
%% For each orphan block for which this block is its prev, run all these steps (including this one) recursively on that orphan
