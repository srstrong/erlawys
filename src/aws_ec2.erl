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
-include_lib("error.hrl").

-import(aws_util, [tuple_3to2/1]).

%-compile(export_all).
-export([init/0,
	 authorize_security_group_ingress/7,
	 authorize_security_group_ingress/9,	
	 confirm_product_instance/6,	
	 create_key_pair/5,	
	 create_security_group/6,	
	 create_tags/6,
	 delete_key_pair/5,	
	 delete_security_group/5,
	 deregister_image/5,
	 describe_image_attribute/6,
	 describe_images/4,
	 describe_images/7,	
	 describe_images_by_tag/6,
	 describe_instance/5,
	 describe_instances/4,
	 describe_instances/5,
	 describe_instances_by_tag/5,
	 describe_key_pairs/5,
	 describe_security_groups/5,
	 get_console_output/5,
	 modify_image_attribute/10,
	 reboot_instance/5,
	 reboot_instances/5,
	 register_image/5,
	 reset_image_attribute/6,
	 revoke_security_group_ingress/7,
	 revoke_security_group_ingress/9,
	 run_instance/5,
	 run_instances/13,
	 start_instance/5,
	 stop_instance/5,
	 terminate_instance/5,
	 terminate_instances/5]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actual AWS API calls.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Initialize AWS APIs.
%%
init() ->
    {ok, Model} = erlsom:compile_xsd_file(filename:join(code:priv_dir(erlawys), "ec2.xsd"), [{prefix, "ec2"}]), 

    {ok, Model2} = erlsom:add_xsd_file(filename:join(code:priv_dir(erlawys), "error.xsd"), [], Model), 

    Model2.

%%
%% AuthorizeSecurityGroupIngress
%%

authorize_security_group_ingress(Region, Key, AccessKey, Model,
				 GroupName,
				 SourceSecurityGroupName,
				 SourceSecurityGroupOwnerId
				) ->
    Xml = aws_ec2_xml:authorize_security_group_ingress(Region, Key, AccessKey,
						       GroupName,
						       SourceSecurityGroupName,
						       SourceSecurityGroupOwnerId,
						       null,
						       null,
						       null,
						       null),
    return_term(Xml, Model).

authorize_security_group_ingress(Region, Key, AccessKey, Model,
				 GroupName,
				 IpProtocol,
				 FromPort,
				 ToPort,
				 CidrIp
				) ->
    Xml = aws_ec2_xml:authorize_security_group_ingress(Region, Key, AccessKey,
						       GroupName,
						       null,
						       null,
						       IpProtocol,
						       FromPort,
						       ToPort,
						       CidrIp),
    return_term(Xml, Model).

%%
%% ConfirmProductInstance
%%

confirm_product_instance(Region, Key, AccessKey, Model,
			 ProductCode,
			 InstanceId
			) ->
    Xml = aws_ec2_xml:confirm_product_instance(Region, Key, AccessKey,
					       ProductCode,
					       InstanceId),
    return_term(Xml, Model).

%%
%% CreateKeyPair
%%

create_key_pair(Region, Key, AccessKey, Model,
		KeyName
	       ) ->
    Xml = aws_ec2_xml:create_key_pair(Region, Key, AccessKey,
				      KeyName),
    return_term(Xml, Model).

%%
%% CreateSecurityGroup
%%

create_security_group(Region, Key, AccessKey, Model,
		      GroupName,
		      GroupDescription
		     ) ->
    Xml = aws_ec2_xml:create_security_group(Region, Key, AccessKey,
					    GroupName,
					    GroupDescription),
    return_term(Xml, Model).

%%
%% CreateTags
%%
create_tags(Region, Key, AccessKey, Model, ResourceId_n, TagKeyValues_n 
	   ) ->
    Xml = aws_ec2_xml:create_tags(Region, Key, AccessKey, ResourceId_n, TagKeyValues_n),
    return_term(Xml, Model).

%%
%% DeleteKeyPair
%%

delete_key_pair(Region, Key, AccessKey, Model,
		KeyName
	       ) ->
    Xml = aws_ec2_xml:delete_key_pair(Region, Key, AccessKey,
				      KeyName),
    return_term(Xml, Model).

%%
%% DeleteSecurityGroup
%%

delete_security_group(Region, Key, AccessKey, Model,
		      GroupName
		     ) ->
    Xml = aws_ec2_xml:delete_security_group(Region, Key, AccessKey,
					    GroupName),
    return_term(Xml, Model).

%%
%% DeregisterImage
%%

deregister_image(Region, Key, AccessKey, Model,
		 ImageId
		) ->
    Xml = aws_ec2_xml:deregister_image(Region, Key, AccessKey,
				       ImageId),
    return_term(Xml, Model).

%%
%% DescribeImageAttribute
%%

describe_image_attribute(Region, Key, AccessKey, Model,
			 ImageId,
			 Attribute
			) ->
    Xml = aws_ec2_xml:describe_image_attribute(Region, Key, AccessKey,
					       ImageId,
					       Attribute),
    return_term(Xml, Model).

%%
%% DescribeImages
%%

describe_images(Region, Key, AccessKey, Model
	       ) ->
    describe_images(Region, Key, AccessKey, Model,
		    [],
		    [],
		    []).

describe_images(Region, Key, AccessKey, Model,
		ImageId_n,
		Owner_n,
		ExecutableBy_n
	       ) ->
    Xml = aws_ec2_xml:describe_images(Region, Key, AccessKey,
				      ImageId_n,
				      Owner_n,
				      ExecutableBy_n),
    return_term(Xml, Model).

describe_images_by_tag(Region, Key, AccessKey, Model, OwnerAlias, Tags
	       ) when (OwnerAlias == self) or (OwnerAlias == amazon) ->
    Xml = aws_ec2_xml:describe_images_by_tag(Region, Key, AccessKey, [atom_to_list(OwnerAlias)], Tags),
    return_term(Xml, Model);

describe_images_by_tag(Region, Key, AccessKey, Model, OwnerAlias, Tags
	       ) when is_list(OwnerAlias) ->
    Xml = aws_ec2_xml:describe_images_by_tag(Region, Key, AccessKey, [OwnerAlias], Tags),
    return_term(Xml, Model).

%%
%% DescribeInstances
%%

describe_instance(Region, Key, AccessKey, Model,
		  InstanceId
		 ) ->
    describe_instances(Region, Key, AccessKey, Model,
		       [InstanceId]).

describe_instances(Region, Key, AccessKey, Model)
-> describe_instances(Region, Key, AccessKey, Model, []).

describe_instances(Region, Key, AccessKey, Model,
		   InstanceId_n
		  ) ->
    Xml = aws_ec2_xml:describe_instances(Region, Key, AccessKey,
					 InstanceId_n),
    return_term(Xml, Model).

describe_instances_by_tag(Region, Key, AccessKey, Model, Tags) ->
    Xml = aws_ec2_xml:describe_instances_by_tag(Region, Key, AccessKey, Tags),
    return_term(Xml, Model).

%%
%% DescribeKeyPairs
%%

describe_key_pairs(Region, Key, AccessKey, Model,
		   KeyName_n
		  ) ->
    Xml = aws_ec2_xml:describe_key_pairs(Region, Key, AccessKey,
					 KeyName_n),
    return_term(Xml, Model).

%%
%% DescribeSecurityGroups
%%

describe_security_groups(Region, Key, AccessKey, Model,
			 GroupName_n
			) ->
    Xml = aws_ec2_xml:describe_security_groups(Region, Key, AccessKey,
					       GroupName_n),
    return_term(Xml, Model).

%%
%% GetConsoleOutput
%%

get_console_output(Region, Key, AccessKey, Model,
		   InstanceId
		  ) ->
    Xml = aws_ec2_xml:get_console_output(Region, Key, AccessKey,
					 InstanceId),
    return_term(Xml, Model).

%%
%% ModifyImageAttribute
%%

modify_image_attribute(Region, Key, AccessKey, Model,
		       ImageId,
		       Attribute,
		       OperationType,
		       UserId_n,
		       UserGroup_n,
		       ProductCode_n
		      ) ->
    Xml = aws_ec2_xml:modify_image_attribute(Region, Key, AccessKey,
					     ImageId,
					     Attribute,
					     OperationType,
					     UserId_n,
					     UserGroup_n,
					     ProductCode_n),
    return_term(Xml, Model).

%%
%% RebootInstances
%%

reboot_instance(Region, Key, AccessKey, Model,
		InstanceId
	       ) ->
    reboot_instances(Region, Key, AccessKey, Model, [InstanceId]).

reboot_instances(Region, Key, AccessKey, Model,
		 InstanceId_n
		) ->
    Xml = aws_ec2_xml:reboot_instances(Region, Key, AccessKey,
				       InstanceId_n),
    return_term(Xml, Model).

%%
%% RegisterImage
%%

register_image(Region, Key, AccessKey, Model,
	       ImageLocation
	      ) ->
    Xml = aws_ec2_xml:register_image(Region, Key, AccessKey,
				     ImageLocation),
    return_term(Xml, Model).

%%
%% ResetImageAttribute
%%

reset_image_attribute(Region, Key, AccessKey, Model,
		      ImageId,
		      Attribute
		     ) ->
    Xml = aws_ec2_xml:reset_image_attribute(Region, Key, AccessKey,
					    ImageId,
					    Attribute),
    return_term(Xml, Model).

%%
%% RevokeSecurityGroupIngress
%%

revoke_security_group_ingress(Region, Key, AccessKey, Model,
			      GroupName,
			      SourceSecurityGroupName,
			      SourceSecurityGroupOwnerId
			     ) ->
    Xml = aws_ec2_xml:revoke_security_group_ingress(Region, Key, AccessKey,
						    GroupName,
						    SourceSecurityGroupName,
						    SourceSecurityGroupOwnerId,
						    null,
						    null,
						    null,
						    null),
    return_term(Xml, Model).

revoke_security_group_ingress(Region, Key, AccessKey, Model,
			      GroupName,
			      IpProtocol,
			      FromPort,
			      ToPort,
			      CidrIp
			     ) ->
    Xml = aws_ec2_xml:revoke_security_group_ingress(Region, Key, AccessKey,
						    GroupName,
						    null,
						    null,
						    IpProtocol,
						    FromPort,
						    ToPort,
						    CidrIp),
    return_term(Xml, Model).

%%
%% RunInstances
%%

run_instance(Region, Key, AccessKey, Model, ImageId) ->
    run_instances(Region, Key, AccessKey, Model, ImageId, 1, 1, null, [], null, null, "m1.small", null).

run_instances(Region, Key, AccessKey, Model,
	      ImageId,
	      MinCount,
	      MaxCount,
	      KeyName,
	      SecurityGroup_n,
	      UserData,
	      AddressingType,
	      InstanceType,
	      AvailabilityZone
	     ) ->
    Xml = aws_ec2_xml:run_instances(Region, Key, AccessKey,
				    ImageId,
				    MinCount,
				    MaxCount,
				    KeyName,
				    SecurityGroup_n,
				    UserData,
				    AddressingType,
				    InstanceType,
				    AvailabilityZone),
    return_term(Xml, Model).

start_instance(Region, Key, AccessKey, Model, InstanceId) ->
    Xml = aws_ec2_xml:start_instances(Region, Key, AccessKey, [InstanceId]),
    return_term(Xml, Model).

stop_instance(Region, Key, AccessKey, Model, InstanceId) ->
    Xml = aws_ec2_xml:stop_instances(Region, Key, AccessKey, [InstanceId]),
    return_term(Xml, Model).

%%
%% TerminateInstances
%%

terminate_instance(Region, Key, AccessKey, Model, InstanceId)
-> terminate_instances(Region, Key, AccessKey, Model, [InstanceId]).

terminate_instances(Region, Key, AccessKey, Model,
		    InstanceId_n
		   ) ->
    Xml = aws_ec2_xml:terminate_instances(Region, Key, AccessKey,
					  InstanceId_n),
    return_term(Xml, Model).

return_term(Xml, Model) ->

    case tuple_3to2(erlsom:scan(Xml, Model)) of
	Result = {ok, _} ->
	    Result;
	Fail = {Other, Term} ->
	    io:format("Got error ~p~n~p~n~p~n", [Other, Term, Xml]),
	    throw({Xml, Fail})
    end.
