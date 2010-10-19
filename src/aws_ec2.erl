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

-module(aws_ec2).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/08/06').

-include_lib("ec2.hrl").

-import(aws_util, [tuple_3to2/1]).

%-compile(export_all).
-export([init/0,
	 authorize_security_group_ingress/6,
	 authorize_security_group_ingress/8,	
	 confirm_product_instance/5,	
	 create_key_pair/4,	
	 create_security_group/5,	
	 create_tags/5,
	 delete_key_pair/4,	
	 delete_security_group/4,
	 deregister_image/4,
	 describe_image_attribute/5,
	 describe_images/3,
	 describe_images/6,	
	 describe_instance/4,
	 describe_instances/3,
	 describe_instances/4,
	 describe_key_pairs/4,
	 describe_security_groups/4,
	 get_console_output/4,
	 modify_image_attribute/9,
	 reboot_instance/4,
	 reboot_instances/4,
	 register_image/4,
	 reset_image_attribute/5,
	 revoke_security_group_ingress/6,
	 revoke_security_group_ingress/8,
	 run_instance/4,
	 run_instances/10,
	 terminate_instance/4,
	 terminate_instances/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
						% Actual AWS API calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Initialize AWS APIs.
%%
init() ->
    {ok, ModelEC2} = erlsom:compile_xsd_file("ec2.xsd", [{prefix, "ec2"}]), ModelEC2.

%%
%% AuthorizeSecurityGroupIngress
%%

authorize_security_group_ingress(Key, AccessKey, Model,
				 GroupName,
				 SourceSecurityGroupName,
				 SourceSecurityGroupOwnerId
				) ->
    Xml = aws_ec2_xml:authorize_security_group_ingress(Key, AccessKey,
						       GroupName,
						       SourceSecurityGroupName,
						       SourceSecurityGroupOwnerId,
						       null,
						       null,
						       null,
						       null),
    tuple_3to2(erlsom:scan(Xml, Model)).

authorize_security_group_ingress(Key, AccessKey, Model,
				 GroupName,
				 IpProtocol,
				 FromPort,
				 ToPort,
				 CidrIp
				) ->
    Xml = aws_ec2_xml:authorize_security_group_ingress(Key, AccessKey,
						       GroupName,
						       null,
						       null,
						       IpProtocol,
						       FromPort,
						       ToPort,
						       CidrIp),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% ConfirmProductInstance
%%

confirm_product_instance(Key, AccessKey, Model,
			 ProductCode,
			 InstanceId
			) ->
    Xml = aws_ec2_xml:confirm_product_instance(Key, AccessKey,
					       ProductCode,
					       InstanceId),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% CreateKeyPair
%%

create_key_pair(Key, AccessKey, Model,
		KeyName
	       ) ->
    Xml = aws_ec2_xml:create_key_pair(Key, AccessKey,
				      KeyName),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% CreateSecurityGroup
%%

create_security_group(Key, AccessKey, Model,
		      GroupName,
		      GroupDescription
		     ) ->
    Xml = aws_ec2_xml:create_security_group(Key, AccessKey,
					    GroupName,
					    GroupDescription),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% CreateTags
%%
create_tags(Key, AccessKey, Model, ResourceId_n, TagKeyValues_n 
	   ) ->
    Xml = aws_ec2_xml:create_tabs(Key, AccessKey, ResourceId_n, TagKeyValues_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DeleteKeyPair
%%

delete_key_pair(Key, AccessKey, Model,
		KeyName
	       ) ->
    Xml = aws_ec2_xml:delete_key_pair(Key, AccessKey,
				      KeyName),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DeleteSecurityGroup
%%

delete_security_group(Key, AccessKey, Model,
		      GroupName
		     ) ->
    Xml = aws_ec2_xml:delete_security_group(Key, AccessKey,
					    GroupName),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DeregisterImage
%%

deregister_image(Key, AccessKey, Model,
		 ImageId
		) ->
    Xml = aws_ec2_xml:deregister_image(Key, AccessKey,
				       ImageId),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DescribeImageAttribute
%%

describe_image_attribute(Key, AccessKey, Model,
			 ImageId,
			 Attribute
			) ->
    Xml = aws_ec2_xml:describe_image_attribute(Key, AccessKey,
					       ImageId,
					       Attribute),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DescribeImages
%%

describe_images(Key, AccessKey, Model
	       ) ->
    describe_images(Key, AccessKey, Model,
		    [],
		    [],
		    []).

describe_images(Key, AccessKey, Model,
		ImageId_n,
		Owner_n,
		ExecutableBy_n
	       ) ->
    Xml = aws_ec2_xml:describe_images(Key, AccessKey,
				      ImageId_n,
				      Owner_n,
				      ExecutableBy_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DescribeInstances
%%

describe_instance(Key, AccessKey, Model,
		  InstanceId
		 ) ->
    describe_instances(Key, AccessKey, Model,
		       [InstanceId]).

describe_instances(Key, AccessKey, Model)
-> describe_instances(Key, AccessKey, Model, []).

describe_instances(Key, AccessKey, Model,
		   InstanceId_n
		  ) ->
    Xml = aws_ec2_xml:describe_instances(Key, AccessKey,
					 InstanceId_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DescribeKeyPairs
%%

describe_key_pairs(Key, AccessKey, Model,
		   KeyName_n
		  ) ->
    Xml = aws_ec2_xml:describe_key_pairs(Key, AccessKey,
					 KeyName_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% DescribeSecurityGroups
%%

describe_security_groups(Key, AccessKey, Model,
			 GroupName_n
			) ->
    Xml = aws_ec2_xml:describe_security_groups(Key, AccessKey,
					       GroupName_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% GetConsoleOutput
%%

get_console_output(Key, AccessKey, Model,
		   InstanceId
		  ) ->
    Xml = aws_ec2_xml:get_console_output(Key, AccessKey,
					 InstanceId),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% ModifyImageAttribute
%%

modify_image_attribute(Key, AccessKey, Model,
		       ImageId,
		       Attribute,
		       OperationType,
		       UserId_n,
		       UserGroup_n,
		       ProductCode_n
		      ) ->
    Xml = aws_ec2_xml:modify_image_attribute(Key, AccessKey,
					     ImageId,
					     Attribute,
					     OperationType,
					     UserId_n,
					     UserGroup_n,
					     ProductCode_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% RebootInstances
%%

reboot_instance(Key, AccessKey, Model,
		InstanceId
	       ) ->
    reboot_instances(Key, AccessKey, Model, [InstanceId]).

reboot_instances(Key, AccessKey, Model,
		 InstanceId_n
		) ->
    Xml = aws_ec2_xml:reboot_instances(Key, AccessKey,
				       InstanceId_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% RegisterImage
%%

register_image(Key, AccessKey, Model,
	       ImageLocation
	      ) ->
    Xml = aws_ec2_xml:register_image(Key, AccessKey,
				     ImageLocation),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% ResetImageAttribute
%%

reset_image_attribute(Key, AccessKey, Model,
		      ImageId,
		      Attribute
		     ) ->
    Xml = aws_ec2_xml:reset_image_attribute(Key, AccessKey,
					    ImageId,
					    Attribute),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% RevokeSecurityGroupIngress
%%

revoke_security_group_ingress(Key, AccessKey, Model,
			      GroupName,
			      SourceSecurityGroupName,
			      SourceSecurityGroupOwnerId
			     ) ->
    Xml = aws_ec2_xml:revoke_security_group_ingress(Key, AccessKey,
						    GroupName,
						    SourceSecurityGroupName,
						    SourceSecurityGroupOwnerId,
						    null,
						    null,
						    null,
						    null),
    tuple_3to2(erlsom:scan(Xml, Model)).

revoke_security_group_ingress(Key, AccessKey, Model,
			      GroupName,
			      IpProtocol,
			      FromPort,
			      ToPort,
			      CidrIp
			     ) ->
    Xml = aws_ec2_xml:revoke_security_group_ingress(Key, AccessKey,
						    GroupName,
						    null,
						    null,
						    IpProtocol,
						    FromPort,
						    ToPort,
						    CidrIp),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% RunInstances
%%

run_instance(Key, AccessKey, Model, ImageId)
-> run_instances(Key, AccessKey, Model, ImageId, 1, 1, null, [], null, null).

run_instances(Key, AccessKey, Model,
	      ImageId,
	      MinCount,
	      MaxCount,
	      KeyName,
	      SecurityGroup_n,
	      UserData,
	      AddressingType
	     ) ->
    Xml = aws_ec2_xml:run_instances(Key, AccessKey,
				    ImageId,
				    MinCount,
				    MaxCount,
				    KeyName,
				    SecurityGroup_n,
				    UserData,
				    AddressingType),
    tuple_3to2(erlsom:scan(Xml, Model)).

%%
%% TerminateInstances
%%

terminate_instance(Key, AccessKey, Model, InstanceId)
-> terminate_instances(Key, AccessKey, Model, [InstanceId]).

terminate_instances(Key, AccessKey, Model,
		    InstanceId_n
		   ) ->
    Xml = aws_ec2_xml:terminate_instances(Key, AccessKey,
					  InstanceId_n),
    tuple_3to2(erlsom:scan(Xml, Model)).

