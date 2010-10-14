%%%
%%% Copyright (c) 2007 Bernhard H. Damberger 
%%% Portions of Comments Copyright(c) 2007 Amazon, Inc.
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

-module(aws_fps_xml).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/08/06').

-include_lib("fps.hrl").

-import(aws_util, [filter_nulls/1, params_signature/2, replace_colons/1, add_default_params/3, create_ec2_param_list/2]).

%-compile(export_all).
-export([fps_url/2,
		get_account_activity/11,
		get_account_balance/2,
		subscribe_for_caller_notification/4,
		un_subscribe_for_caller_notification/3,
		cancel_token/4,
		get_payment_instruction/3,
		get_token_by_caller/4,
		get_tokens/4,
		get_token_usage/3,
		install_payment_instruction/7,
		get_all_credit_instruments/3,
		get_debt_balance/3,
		get_out_standing_debt_balance/2,
		settle_debt/15,
		write_off_debt/11,
		fund_prepaid/15,
		get_all_prepaid_instruments/3,
		get_prepaid_balance/3,
		get_total_prepaid_liability/2,
		discard_results/3,
		get_results/4,
		get_transaction/3,
		pay/15,
		refund/15,
		reserve/15,
		retry_transaction/3,
		settle/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Methods used to construct URLs to access AWS.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FPS_SANDBOX_URL, "https://fps.sandbox.amazonaws.com").
-define(VERSION, "2007-01-08").

% Construct the URL for accessing a web services from ec2.
fps_url(Key, Params) -> fps_url(Key, Params, ?FPS_SANDBOX_URL).
fps_url(Key, Params, Url) ->
	NoNullParams = filter_nulls(Params),
	Url ++ fps_url_1([{"Signature", params_signature(Key, NoNullParams)}|lists:reverse(NoNullParams)], []).
%fps_url(Key, Params) -> Url = ?FPS_SANDBOX_URL ++ fps_url_1(lists:reverse(Params), []), io:format("~p", Url), Url.

fps_url_1([{K, V}], Data) -> fps_url_1([], ["?", K, "=", replace_colons(V) | Data]);
fps_url_1([{K, V}|T], Data) -> fps_url_1(T, ["&", K, "=", replace_colons(V) | Data]);
fps_url_1([], Data) -> lists:flatten(Data).

add_default_params(Params, AccessKey) -> add_default_params(Params, AccessKey, ?VERSION).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual AWS API calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GetAccountActivity
%%
%% You can use the GetAccountActivity operation to retrieve transactions from an account for a given time period. You can customize the results using the various request parameters listed in this topic. 
%% 

get_account_activity(Key, AccessKey,
		StartDate,
		EndDate,
		MaxBatchSize,
		SortOrderByDate,
		ResponseGroup,
		Operation,
		PaymentMethod,
		Role,
		Status
	) ->
	Params = add_default_params(
		[{"Action", "GetAccountActivity"},
		{"StartDate", StartDate},
		{"EndDate", EndDate},
		{"MaxBatchSize", MaxBatchSize},
		{"SortOrderByDate", SortOrderByDate},
		{"ResponseGroup", ResponseGroup},
		{"Operation", Operation},
		{"PaymentMethod", PaymentMethod},
		{"Role", Role},
		{"Status", Status}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetAccountBalance
%%
%% You can use the GetAccountBalance operation to get the current balance on your account. 
%% 

get_account_balance(Key, AccessKey
	) ->
	Params = add_default_params(
		[{"Action", "GetAccountBalance"}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% SubscribeForCallerNotification
%%
%% The SubscribeForCallerNotification operation allows callers to subscribe to events that are given out using the web service notification mechanism. This operation is used for subscribing to the notifications provided to callers through web services. Amazon FPS supports two events, Transaction results and token deletion that you can subscribe. 
%% 

subscribe_for_caller_notification(Key, AccessKey,
		NotificationOperationName,
		WebServiceAPIURL
	) ->
	Params = add_default_params(
		[{"Action", "SubscribeForCallerNotification"},
		{"NotificationOperationName", NotificationOperationName},
		{"WebServiceAPIURL", WebServiceAPIURL}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% UnSubscribeForCallerNotification
%%
%% The UnSubscribeForCallerNotification operation allows callers to unsubscribe to events that are previously subscribed by the calling applications. 
%% 

un_subscribe_for_caller_notification(Key, AccessKey,
		NotificationOperationName
	) ->
	Params = add_default_params(
		[{"Action", "UnSubscribeForCallerNotification"},
		{"NotificationOperationName", NotificationOperationName}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% CancelToken
%%
%% You can use the CancelToken operation to cancel any token that you installed on your own account. 

cancel_token(Key, AccessKey,
		TokenId,
		ReasonText
	) ->
	Params = add_default_params(
		[{"Action", "CancelToken"},
		{"TokenId", TokenId},
		{"ReasonText", ReasonText}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetPaymentInstruction
%%
%% The GetPaymentInstruction operation gives the details of a payment instruction that is referenced by the TokenId. 
%% 
%% Note 
%% You can query for tokens installed only on your own account. 
%% 
%% 

get_payment_instruction(Key, AccessKey,
		TokenId
	) ->
	Params = add_default_params(
		[{"Action", "GetPaymentInstruction"},
		{"TokenId", TokenId}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetTokenByCaller
%%
%% You can use the GetTokenByCaller operation to fetch a list of tokens and the details of a particular token you installed using the Amazon FPS co-branded UI pipeline. You must pass either the token Id or the caller reference to retrieve the tokens. 
%% 

get_token_by_caller(Key, AccessKey,
		TokenId,
		CallerReference
	) ->
	Params = add_default_params(
		[{"Action", "GetTokenByCaller"},
		{"TokenId", TokenId},
		{"CallerReference", CallerReference}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetTokens
%%
%% The GetTokens operation is used to fetch all the tokens installed on your (caller) account. 
%% 

get_tokens(Key, AccessKey,
		TokenFriendlyName,
		TokenStatus
	) ->
	Params = add_default_params(
		[{"Action", "GetTokens"},
		{"TokenFriendlyName", TokenFriendlyName},
		{"TokenStatus", TokenStatus}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetTokenUsage
%%
%% You can use the GetTokenUsage operation to fetch the details and usage of a multi-use token. A multi-use token has GateKeeper constructs, that allow Amazon FPS to monitor the token usage. Amazon FPS returns the token usages that are configured in the GateKeeper constructs. If only one of the token usage limits is configured, only that particular field is returned. 
%% 
%% Note 
%% The usage limit is returned only for the multi-use token and not for the single use token. 
%% 

get_token_usage(Key, AccessKey,
		TokenId
	) ->
	Params = add_default_params(
		[{"Action", "GetTokenUsage"},
		{"TokenId", TokenId}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% InstallPaymentInstruction
%%
%% You can use the InstallPaymentInstruction operation to install tokens (payment instructions) on your own accounts. 
%% 
%% See the Core Concepts section in the Amazon FPS Getting Started Guide for more information on tokens or payment instructions.

install_payment_instruction(Key, AccessKey,
		PaymentInstructions,
		CallerReference,
		FriendlyName,
		TokenType,
		PaymentReason
	) ->
	Params = add_default_params(
		[{"Action", "InstallPaymentInstruction"},
		{"PaymentInstructions", PaymentInstructions},
		{"CallerReference", CallerReference},
		{"FriendlyName", FriendlyName},
		{"TokenType", TokenType},
		{"PaymentReason", PaymentReason}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetAllCreditInstruments
%%
%% The GetAllCreditInstruments function is used to retrieve all credit instruments associated with an account. 

get_all_credit_instruments(Key, AccessKey,
		InstrumentStatus
	) ->
	Params = add_default_params(
		[{"Action", "GetAllCreditInstruments"},
		{"InstrumentStatus", InstrumentStatus}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetDebtBalance
%%
%% The GetDebtBalance is used to retrieve the balance of a credit instrument. You can query for the debt balance only on the instruments for which you are the sender or the recipient. 
%% 

get_debt_balance(Key, AccessKey,
		CreditInstrumentId
	) ->
	Params = add_default_params(
		[{"Action", "GetDebtBalance"},
		{"CreditInstrumentId", CreditInstrumentId}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetOutStandingDebtBalance
%%
%% The GetOutStandingDebtBalance is used to retrieve balances of all credit instruments owned by the sender. 
%% 

get_out_standing_debt_balance(Key, AccessKey
	) ->
	Params = add_default_params(
		[{"Action", "GetOutStandingDebtBalance"}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% SettleDebt
%%
%% When the credit Instrument is created on Amazon FPS using the co-branded UI pipeline, you get a settlement token as an output from the pipeline. A settlement token is a token that is installed on the sender's account, which gives a payment method and the instrument that is used to settle the debt balance accumulated by the sender. 
%% 
%% The SettleDebt operation takes the settlement amount, credit instrument, and the settlement token among other parameters. Using this operation you can: 
%% 
%% * Transfer money from sender's payment instrument specified in the settlement token to the recipient's account balance. The fee charged is deducted from the settlement amount and deposited into recipient's account balance. 
%% 
%% * Decrement debt balances by the settlement amount. 
%% 
%% Example 
%% For example, if the debt balance is $100, settlement amount is $90, and Amazon FPS fee is $2.50 then settle debt moves $87.50 into recipient's account balance and decrements debt balance by $90 making it $10. 
%% 
%% Note 
%% * The SettleDebt operation cannot be used for amounts greater than the actual debt balance. For example, if the debt balance is $100, you cannot use SettleDebt operation for $110. 
%% 
%% * The Refund operation does not support SettleDebt operation. Settlement tokens can use any of the following payment methods: 
%% 
%% * Credit cards 
%% 
%% * Bank accounts (It takes about 4 days to receive funds from a bank account transfer.) 
%% 
%% * Account balance 
%% 
%% Note 
%% The settlement tokens can be used multiple times to settle debt. A maximum limit has to be imposed on the total value of such settlements that will happen using the settlement token. Amazon FPS shows this limit to senders when they setup credit instrument using co-branded UI. Sender set up authorizationthat allows you to charge them multiple times with in the maximum limit. 
%% 

settle_debt(Key, AccessKey,
		SenderTokenId,
		CallerTokenId,
		CreditInstrumentId,
		SettlementAmount,
		TransactionDate,
		SenderReference,
		RecipientReference,
		CallerReference,
		ChargeFeeTo,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "SettleDebt"},
		{"SenderTokenId", SenderTokenId},
		{"CallerTokenId", CallerTokenId},
		{"CreditInstrumentId", CreditInstrumentId},
		{"SettlementAmount", SettlementAmount},
		{"TransactionDate", TransactionDate},
		{"SenderReference", SenderReference},
		{"RecipientReference", RecipientReference},
		{"CallerReference", CallerReference},
		{"ChargeFeeTo", ChargeFeeTo},
		{"SenderDescription", SenderDescription},
		{"RecipientDescription", RecipientDescription},
		{"CallerDescription", CallerDescription},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% WriteOffDebt
%%
%% You can use the WriteOffDebt operation to write off the debt accumulated by the recipient on any credit instrument. 
%% 

write_off_debt(Key, AccessKey,
		CallerTokenId,
		CreditInstrumentId,
		AdjustmentAmount,
		TransactionDate,
		SenderReference,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "WriteOffDebt"},
		{"CallerTokenId", CallerTokenId},
		{"CreditInstrumentId", CreditInstrumentId},
		{"AdjustmentAmount", AdjustmentAmount},
		{"TransactionDate", TransactionDate},
		{"SenderReference", SenderReference},
		{"SenderDescription", SenderDescription},
		{"RecipientDescription", RecipientDescription},
		{"CallerDescription", CallerDescription},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% FundPrepaid
%%
%% The FundPrepaid operation is used to transfer money from sender's payment instrument specified in the funding token to the recipient's account balance and create a prepaid balance on the prepaid instrument for the sender. 
%% 
%% Note 
%% Recipient token is not passed as input to the FundPrepaid operation because the recipient is associated with the prepaid instrument and that represents the recipient. 
%% 
%% The sender funds the prepaid balance of a particular prepaid instrument that is created using Amazon FPS co-branded UI pipelines. The prepaid balance can be funded using a credit card, ACH or Amazon Payments account balance. This is specified by the funding token. The Funding token is a sender token that is used for funding the prepaid instrument. It has an additional GateKeeper constraint on the prepaid instrument that is to be funded. 
%% 
%% 

fund_prepaid(Key, AccessKey,
		SenderTokenId,
		CallerTokenId,
		PrepaidInstrumentId,
		FundingAmount,
		TransactionDate,
		SenderReference,
		RecipientReference,
		CallerReference,
		ChargeFeeTo,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "FundPrepaid"},
		{"SenderTokenId", SenderTokenId},
		{"CallerTokenId", CallerTokenId},
		{"PrepaidInstrumentId", PrepaidInstrumentId},
		{"FundingAmount", FundingAmount},
		{"TransactionDate", TransactionDate},
		{"SenderReference", SenderReference},
		{"RecipientReference", RecipientReference},
		{"CallerReference", CallerReference},
		{"ChargeFeeTo", ChargeFeeTo},
		{"SenderDescription", SenderDescription},
		{"RecipientDescription", RecipientDescription},
		{"CallerDescription", CallerDescription},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetAllPrepaidInstruments
%%
%% The GetAllPrepaidInstruments operation is used to return all the prepaid instruments associated with your account. 
%% 

get_all_prepaid_instruments(Key, AccessKey,
		InstrumentStatus
	) ->
	Params = add_default_params(
		[{"Action", "GetAllPrepaidInstruments"},
		{"InstrumentStatus", InstrumentStatus}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetPrepaidBalance
%%
%% The GetPrepaidBalance operation is used to retrieve the balance of a prepaid instrument. 
%% 
%% Note 
%% You can query for the prepaid balance only if you are the sender or the recipient for the prepaid instrument. 
%% 

get_prepaid_balance(Key, AccessKey,
		PrepaidInstrumentId
	) ->
	Params = add_default_params(
		[{"Action", "GetPrepaidBalance"},
		{"PrepaidInstrumentId", PrepaidInstrumentId}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetTotalPrepaidLiability
%%
%% The GetTotalPrepaidLiability operation returns the total liability held by the recipient corresponding to all the prepaid instruments.

get_total_prepaid_liability(Key, AccessKey
	) ->
	Params = add_default_params(
		[{"Action", "GetTotalPrepaidLiability"}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DiscardResults
%%
%% The DiscardResults operation is used to discard the results that are fetched using the GetResults operation. 
%% 
%% Amazon FPS provides the GetResults operation to fetch the results of the completed transactions. The results remain in the queue until the transactions are explicitly removed from the queue. The transactions are held in the queue until they are explicitly removed using the DiscardResults operation. 
%% 

discard_results(Key, AccessKey,
		TransactionIds
	) ->
	Params = add_default_params(
		[{"Action", "DiscardResults"},
		{"TransactionIds", TransactionIds}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetResults
%%
%% Amazon FPS transactions that involve an external payment instruments (CC and ACH) are not completed synchronously during the web service call. The GetResults operation is used to poll for results that are returned asynchronously. 
%% 
%% You can view details of individual transactions using the GetTransaction operation. However, to find the status of the transactions, you should use the GetResults operation. 
%% 

get_results(Key, AccessKey,
		Operation,
		MaxResultCount
	) ->
	Params = add_default_params(
		[{"Action", "GetResults"},
		{"Operation", Operation},
		{"MaxResultCount", MaxResultCount}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetTransaction
%%
%% The GetTransaction operation is used to fetch details of a transaction referred by the TokenID. 
%% 
%% Note 
%% You can query only transactions on your account. 
%% 

get_transaction(Key, AccessKey,
		TransactionId
	) ->
	Params = add_default_params(
		[{"Action", "GetTransaction"},
		{"TransactionId", TransactionId}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% Pay
%%
%% The Pay operation transfers money from a sender to a recipient. This operation uses three tokens—a sender token, a recipient token, and a caller token; which together represent the authorization from each of the participants to execute the transaction. See Creating Tokens for more information on creating and obtaining tokens. 
%% 
%% The sender token defines the payment method to be used in the transaction. If the payment method used for the sender token is ABT, prepaid, or postpaid, then the transaction is completed synchronously. If the payment method is a bank account (ACH) or a credit card (CC), then the transaction is completed asynchronously. See handling synchronous and asynchronous responses section in the Programming Reference for more information. 
%% 

pay(Key, AccessKey,
		RecipientTokenID,
		SenderTokenId,
		CallerTokenId,
		TransactionAmount,
		TransactionDate,
		SenderReference,
		RecipientReference,
		CallerReference,
		ChargeFeeTo,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "Pay"},
		{"RecipientTokenID", RecipientTokenID},
		{"SenderTokenId", SenderTokenId},
		{"CallerTokenId", CallerTokenId},
		{"TransactionAmount", TransactionAmount},
		{"TransactionDate", TransactionDate},
		{"SenderReference", SenderReference},
		{"RecipientReference", RecipientReference},
		{"CallerReference", CallerReference},
		{"ChargeFeeTo", ChargeFeeTo},
		{"SenderDescription", SenderDescription},
		{"RecipientDescription", RecipientDescription},
		{"CallerDescription", CallerDescription},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% Refund
%%
%% You can use the Refund operation to refund a successfully completed payment transaction. See the Refund section for rules on issuing a refund. In case of asynchronous refund you can use the 
%% 
%% GetResults and the DiscardResults operation to handle the responses. 
%% The Refund operation can be used only by the caller of the original transaction. 
%% 

refund(Key, AccessKey,
		TransactionId,
		RefundAmount,
		CallerTokenId,
		RefundSenderTokenId,
		ChargeFeeTo,
		RefundSenderReference,
		RefundRecipientReference,
		CallerReference,
		RefundSenderDescription,
		RefundRecipientDescription,
		CallerDescription,
		TransactionDate,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "Refund"},
		{"TransactionId", TransactionId},
		{"RefundAmount", RefundAmount},
		{"CallerTokenId", CallerTokenId},
		{"RefundSenderTokenId", RefundSenderTokenId},
		{"ChargeFeeTo", ChargeFeeTo},
		{"RefundSenderReference", RefundSenderReference},
		{"RefundRecipientReference", RefundRecipientReference},
		{"CallerReference", CallerReference},
		{"RefundSenderDescription", RefundSenderDescription},
		{"RefundRecipientDescription", RefundRecipientDescription},
		{"CallerDescription", CallerDescription},
		{"TransactionDate", TransactionDate},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% Reserve
%%
%% The Reserve operation is part of the Reserve and Settle operations that allow payment transactions where the authorization and settlement have a time difference. The transaction is not complete until the Settle operation is executed successfully. A reserve authorization is only valid for 7 days. Currently, there is no cancellation for a reserve. 
%% 
%% Read the Reserve and Settle section for more information on how the reserve and settle transactions work. 
%% 
%% Note 
%% You can only settle once for a reserve operation. 
%% 

reserve(Key, AccessKey,
		RecipientTokenId,
		SenderTokenId,
		CallerTokenId,
		SenderReference,
		RecipientReference,
		CallerReference,
		TransactionDate,
		TransactionAmount,
		ChargeFeeTo,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData
	) ->
	Params = add_default_params(
		[{"Action", "Reserve"},
		{"RecipientTokenId", RecipientTokenId},
		{"SenderTokenId", SenderTokenId},
		{"CallerTokenId", CallerTokenId},
		{"SenderReference", SenderReference},
		{"RecipientReference", RecipientReference},
		{"CallerReference", CallerReference},
		{"TransactionDate", TransactionDate},
		{"TransactionAmount", TransactionAmount},
		{"ChargeFeeTo", ChargeFeeTo},
		{"SenderDescription", SenderDescription},
		{"RecipientDescription", RecipientDescription},
		{"CallerDescription", CallerDescription},
		{"MetaData", MetaData}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% RetryTransaction
%%
%% The RetryTransaction operation is used to re-initiate a transaction that was temporary declined earlier. Only the Pay and Reserve operations can return a TemporaryDecline status. 
%% 

retry_transaction(Key, AccessKey,
		OriginalTransactionId 
	) ->
	Params = add_default_params(
		[{"Action", "RetryTransaction"},
		{"OriginalTransactionId ", OriginalTransactionId }],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% Settle
%%
%% The Settle operation settles fully or partially the amount that is reserved using the Reserve operation. 
%% 
%% Read the Reserve and Settle Use Case section for more information on how the settle function works. 
%% 

settle(Key, AccessKey,
		SettlementAmount,
		TransactionId,
		TransactionDate
	) ->
	Params = add_default_params(
		[{"Action", "Settle"},
		{"SettlementAmount", SettlementAmount},
		{"TransactionId", TransactionId},
		{"TransactionDate", TransactionDate}],
		AccessKey),
	Url = fps_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
	
