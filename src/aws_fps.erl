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

-module(aws_fps).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/08/06').

-include_lib("fps.hrl").

-import(aws_util, [tuple_3to2/1]).

%-compile(export_all).
-export([init/0,
		get_account_activity/12,	
		get_account_balance/3,	
		subscribe_for_caller_notification/5,	
		un_subscribe_for_caller_notification/4,	
		cancel_token/5,	
		get_payment_instruction/4,	
		get_token_by_caller/5,	
		get_tokens/5,	
		get_token_usage/4,	
		install_payment_instruction/8,	
		get_all_credit_instruments/4,	
		get_debt_balance/4,	
		get_out_standing_debt_balance/3,	
		settle_debt/16,	
		write_off_debt/12,	
		fund_prepaid/16,	
		get_all_prepaid_instruments/4,	
		get_prepaid_balance/4,	
		get_total_prepaid_liability/3,	
		discard_results/4,	
		get_results/5,	
		get_transaction/4,	
		pay/16,	
		refund/16,	
		reserve/16,	
		retry_transaction/4,	
		settle/6]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Actual AWS API calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Initialize AWS APIs.
%%
init() ->
	{ok, ModelFPS} = erlsom:compile_xsd_file("fps.xsd", [{prefix, "fps"}]), ModelFPS.

%%
%% GetAccountActivity
%%

get_account_activity(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:get_account_activity(Key, AccessKey,
		StartDate,
		EndDate,
		MaxBatchSize,
		SortOrderByDate,
		ResponseGroup,
		Operation,
		PaymentMethod,
		Role,
		Status),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetAccountBalance
%%

get_account_balance(Key, AccessKey, Model
	) ->
	Xml = aws_fps_xml:get_account_balance(Key, AccessKey),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% SubscribeForCallerNotification
%%

subscribe_for_caller_notification(Key, AccessKey, Model,
		NotificationOperationName,
		WebServiceAPIURL
	) ->
	Xml = aws_fps_xml:subscribe_for_caller_notification(Key, AccessKey,
		NotificationOperationName,
		WebServiceAPIURL),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% UnSubscribeForCallerNotification
%%

un_subscribe_for_caller_notification(Key, AccessKey, Model,
		NotificationOperationName
	) ->
	Xml = aws_fps_xml:un_subscribe_for_caller_notification(Key, AccessKey,
		NotificationOperationName),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% CancelToken
%%

cancel_token(Key, AccessKey, Model,
		TokenId,
		ReasonText
	) ->
	Xml = aws_fps_xml:cancel_token(Key, AccessKey,
		TokenId,
		ReasonText),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetPaymentInstruction
%%

get_payment_instruction(Key, AccessKey, Model,
		TokenId
	) ->
	Xml = aws_fps_xml:get_payment_instruction(Key, AccessKey,
		TokenId),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetTokenByCaller
%%

get_token_by_caller(Key, AccessKey, Model,
		TokenId,
		CallerReference
	) ->
	Xml = aws_fps_xml:get_token_by_caller(Key, AccessKey,
		TokenId,
		CallerReference),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetTokens
%%

get_tokens(Key, AccessKey, Model,
		TokenFriendlyName,
		TokenStatus
	) ->
	Xml = aws_fps_xml:get_tokens(Key, AccessKey,
		TokenFriendlyName,
		TokenStatus),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetTokenUsage
%%

get_token_usage(Key, AccessKey, Model,
		TokenId
	) ->
	Xml = aws_fps_xml:get_token_usage(Key, AccessKey,
		TokenId),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% InstallPaymentInstruction
%%

install_payment_instruction(Key, AccessKey, Model,
		PaymentInstructions,
		CallerReference,
		FriendlyName,
		TokenType,
		PaymentReason
	) ->
	Xml = aws_fps_xml:install_payment_instruction(Key, AccessKey,
		PaymentInstructions,
		CallerReference,
		FriendlyName,
		TokenType,
		PaymentReason),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetAllCreditInstruments
%%

get_all_credit_instruments(Key, AccessKey, Model,
		InstrumentStatus
	) ->
	Xml = aws_fps_xml:get_all_credit_instruments(Key, AccessKey,
		InstrumentStatus),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetDebtBalance
%%

get_debt_balance(Key, AccessKey, Model,
		CreditInstrumentId
	) ->
	Xml = aws_fps_xml:get_debt_balance(Key, AccessKey,
		CreditInstrumentId),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetOutStandingDebtBalance
%%

get_out_standing_debt_balance(Key, AccessKey, Model
	) ->
	Xml = aws_fps_xml:get_out_standing_debt_balance(Key, AccessKey
),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% SettleDebt
%%

settle_debt(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:settle_debt(Key, AccessKey,
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
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% WriteOffDebt
%%

write_off_debt(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:write_off_debt(Key, AccessKey,
		CallerTokenId,
		CreditInstrumentId,
		AdjustmentAmount,
		TransactionDate,
		SenderReference,
		SenderDescription,
		RecipientDescription,
		CallerDescription,
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% FundPrepaid
%%

fund_prepaid(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:fund_prepaid(Key, AccessKey,
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
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetAllPrepaidInstruments
%%

get_all_prepaid_instruments(Key, AccessKey, Model,
		InstrumentStatus
	) ->
	Xml = aws_fps_xml:get_all_prepaid_instruments(Key, AccessKey,
		InstrumentStatus),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetPrepaidBalance
%%

get_prepaid_balance(Key, AccessKey, Model,
		PrepaidInstrumentId
	) ->
	Xml = aws_fps_xml:get_prepaid_balance(Key, AccessKey,
		PrepaidInstrumentId),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetTotalPrepaidLiability
%%

get_total_prepaid_liability(Key, AccessKey, Model
	) ->
	Xml = aws_fps_xml:get_total_prepaid_liability(Key, AccessKey
),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% DiscardResults
%%

discard_results(Key, AccessKey, Model,
		TransactionIds
	) ->
	Xml = aws_fps_xml:discard_results(Key, AccessKey,
		TransactionIds),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetResults
%%

get_results(Key, AccessKey, Model,
		Operation,
		MaxResultCount
	) ->
	Xml = aws_fps_xml:get_results(Key, AccessKey,
		Operation,
		MaxResultCount),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% GetTransaction
%%

get_transaction(Key, AccessKey, Model,
		TransactionId
	) ->
	Xml = aws_fps_xml:get_transaction(Key, AccessKey,
		TransactionId),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% Pay
%%

pay(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:pay(Key, AccessKey,
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
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% Refund
%%

refund(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:refund(Key, AccessKey,
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
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% Reserve
%%

reserve(Key, AccessKey, Model,
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
	Xml = aws_fps_xml:reserve(Key, AccessKey,
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
		MetaData),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% RetryTransaction
%%

retry_transaction(Key, AccessKey, Model,
		OriginalTransactionId 
	) ->
	Xml = aws_fps_xml:retry_transaction(Key, AccessKey,
		OriginalTransactionId ),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
%%
%% Settle
%%

settle(Key, AccessKey, Model,
		SettlementAmount,
		TransactionId,
		TransactionDate
	) ->
	Xml = aws_fps_xml:settle(Key, AccessKey,
		SettlementAmount,
		TransactionId,
		TransactionDate),
	tuple_3to2(erlsom:scan(Xml, Model)).
	
