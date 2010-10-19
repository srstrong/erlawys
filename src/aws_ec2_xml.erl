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

-module(aws_ec2_xml).
-author('bernied@gmail.com').
-vcn('0.1').
-date('2007/07/28').

-include_lib("ec2.hrl").

-import(aws_util, [filter_nulls/1, params_signature/2, replace_colons/1, add_default_params/3, create_ec2_param_list/2, create_ec2_param_list/3]).

%-compile(export_all).
-export([ec2_url/2,
		authorize_security_group_ingress/5,	
		authorize_security_group_ingress/7,
		confirm_product_instance/4,	
		create_key_pair/3,	
		create_security_group/4,	
		delete_key_pair/3,	
		delete_security_group/3,	
		deregister_image/3,	
		describe_image_attribute/4,	
		describe_images/5,	
		describe_instances/3,	
		describe_key_pairs/3,	
		describe_security_groups/3,	
		get_console_output/3,	
		modify_image_attribute/8,	
		reboot_instances/3,	
		register_image/3,	
		reset_image_attribute/4,	
		revoke_security_group_ingress/5,	
		revoke_security_group_ingress/7,	
		run_instances/9,	
		terminate_instances/3,
	        create_tags/4]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper Methods used to construct URLs to access AWS.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EC2_BASE_URL, "https://ec2.amazonaws.com/").
%-define(VERSION, "2007-03-01").
-define(VERSION, "2010-08-31").


% Construct the URL for accessing a web services from ec2.
ec2_url(Key, Params) ->
	NoNullParams = filter_nulls(Params),
	?EC2_BASE_URL ++ ec2_url_1([{"Signature", params_signature(Key, NoNullParams)}|lists:reverse(NoNullParams)], []).


ec2_url_1([{K, V}], Data) -> ec2_url_1([], ["?", K, "=", edoc_lib:escape_uri(V) | Data]);
ec2_url_1([{K, V}|T], Data) -> ec2_url_1(T, ["&", K, "=", edoc_lib:escape_uri(V) | Data]);
ec2_url_1([], Data) -> lists:flatten(Data).

add_default_params(Params, AccessKey) -> add_default_params(Params, AccessKey, ?VERSION).

create_tags(Key, AccessKey, ResourceId_n, TagKeyValues_n 
	) ->
        ExpandedTags = [Tags || _Resources <- ResourceId_n, Tags <- TagKeyValues_n],
	Params = add_default_params(
		lists:flatten([
			       {"Action", "CreateTags"},
			       create_ec2_param_list("ResourceId", ResourceId_n),
			       create_ec2_param_list("Tag", "Key", lists:map(fun({TagKey, _}) -> TagKey end, ExpandedTags)),
			       create_ec2_param_list("Tag", "Value", lists:map(fun({_, TagValue}) -> TagValue end, ExpandedTags))
			      ]),
		AccessKey),
	Url = ec2_url(Key, Params),
io:format("~p~n", [Url]),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Implementation of Amazon EC2 API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% AuthorizeSecurityGroupIngress
%%
%% The AuthorizeSecurityGroupIngress operation adds permissions to a security
%% group.
%% 
%% Permissions are specified in terms of the IP protocol (TCP, UDP or ICMP), 
%% the source of the request (by IP range or an Amazon EC2 user-group pair),
%% source and destination port ranges (for TCP and UDP), and ICMP codes and
%% types (for ICMP). When authorizing ICMP, -1 may be used as a wildcard in the
%% type and code fields.
%% 
%% Permission changes are propagated to instances within the security group
%% being modified as quickly as possible. However, a small delay is likely,
%% depending on the number of instances that are members of the indicated group.
%% 
%% When authorizing a user/group pair permission, GroupName,
%% SourceSecurityGroupName and SourceSecurityGroupOwnerId must be
%% specified. When authorizing a CIDR IP permission, GroupName, IpProtocol,
%% FromPort, ToPort and CidrIp must be specified. Mixing these two types of
%% parameters is not allowed. 

authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId
	) ->
	authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId,
		null,
		null,
		null,
		null).
		
authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		null,
		null,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp).

authorize_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	Params = add_default_params(
		[{"Action", "AuthorizeSecurityGroupIngress"},
		{"Groupname", GroupName},
		{"SourceSecurityGroupName", SourceSecurityGroupName},
		{"SourceSecurityGroupOwnerId", SourceSecurityGroupOwnerId},
		{"IpProtocol", IpProtocol},
		{"FromPort", FromPort},
		{"ToPort", ToPort},
		{"CidrIp", CidrIp}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.

%% ConfirmProductInstance
%%
%% The ConfirmProductInstance operation returns true if the given product code is
%% attached to the instance with the given instance id. The operation returns false if
%% the product code is not attached to the instance.
%% 
%% The ConfirmProductInstance operation can only be executed by the owner of
%% the AMI. This feature is useful when an AMI owner is providing support and
%% wants to verify whether a user's instance is eligible. 

confirm_product_instance(Key, AccessKey,
		ProductCode,
		InstanceId
	) ->
	Params = add_default_params(
		[{"Action", "ConfirmProductInstance"},
		{"ProductCode", ProductCode},
		{"InstanceId", InstanceId}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% CreateKeyPair
%%
%% The CreateKeyPair operation creates a new 2048 bit RSA keypair and returns a
%% unique ID that can be used to reference this keypair when launching new
%% instances.

create_key_pair(Key, AccessKey,
		KeyName
	) ->
	Params = add_default_params(
		[{"Action", "CreateKeyPair"},
		{"KeyName", KeyName}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% CreateSecurityGroup
%%
%% The CreateSecurityGroup operation creates a new security group.
%% 
%% Every instance is launched in a security group. If none is specified as part of the
%% launch request then instances are launched in the default security group.
%% Instances within the same security group have unrestricted network access to
%% one another. Instances will reject network access attempts from other instances
%% in a different security group. As the owner of instances you may grant or revoke
%% specific permissions using the AuthorizeSecurityGroupIngress and
%% RevokeSecurityGroupIngress operations. 

create_security_group(Key, AccessKey,
		GroupName,
		GroupDescription
	) ->
	Params = add_default_params(
		[{"Action", "CreateSecurityGroup"},
		{"GroupName", GroupName},
		{"GroupDescription", GroupDescription}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DeleteKeyPair
%%
%% The DeleteKeyPair operation deletes a keypair.

delete_key_pair(Key, AccessKey,
		KeyName
	) ->
	Params = add_default_params(
		[{"Action", "DeleteKeyPair"},
		{"KeyName", KeyName}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DeleteSecurityGroup
%%
%% The DeleteSecurityGroup operation deletes a security group.
%% 
%% If an attempt is made to delete a security group and any instances exist that are
%% members of that group a fault is returned.
%% 

delete_security_group(Key, AccessKey,
		GroupName
	) ->
	Params = add_default_params(
		[{"Action", "DeleteSecurityGroup"},
		{"GroupName", GroupName}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DeregisterImage
%%
%% The DeregisterImage operation deregisters an AMI. Once deregistered,
%% instances of the AMI may no longer be launched.

deregister_image(Key, AccessKey,
		ImageId
	) ->
	Params = add_default_params(
		[{"Action", "DeregisterImage"},
		{"ImageId", ImageId}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DescribeImageAttribute
%%
%% The DescribeImageAttribute operation returns information about an attribute of
%% an AMI. Only one attribute may be specified per call.

describe_image_attribute(Key, AccessKey,
		ImageId,
		Attribute
	) ->
	Params = add_default_params(
		[{"Action", "DescribeImageAttribute"},
		{"ImageId", ImageId},
		{"Attribute", Attribute}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DescribeImages
%%
%%  The DescribeImages operation returns information about AMIs available for
%% use by the user. This includes both public AMIs (those available for any user to
%% launch) and private AMIs (those owned by the user making the request and
%% those owned by other users that the user making the request has explicit launch
%% permissions for).
%% 
%% The list of AMIs returned can be modified through optional lists of AMI IDs,
%% owners or users with launch permissions. If all three optional lists are empty all
%% AMIs the user has launch permissions for are returned. Launch permissions fall
%% into three categories:
%% 
%% public:
%%   The all group has launch permissions for the AMI. All users have launch
%% permissions for these AMIs.
%% explicit:
%%   The owner of the AMIs has granted a specific user launch permissions for the
%% AMI.
%% implicit:
%%   A user has implicit launch permissions for all AMIs he or she owns.
%% 
%% If one or more of the lists are specified the result set is the intersection of AMIs
%% matching the criteria of the individual lists.
%% 
%% Providing the list of AMI IDs requests information for those AMIs only. If no AMI
%% IDs are provided, information of all relevant AMIs will be returned. If an AMI is
%% specified that does not exist a fault is returned. If an AMI is specified that exists
%% but the user making the request does not have launch permissions for, then that
%% AMI will not be included in the returned results.
%% 
%% Providing the list of owners requests information for AMIs owned by the
%% specified owners only. Only AMIs the user has launch permissions for are
%% returned. The items of the list may be account ids for AMIs owned by users with
%% those account ids, amazon for AMIs owned by Amazon or self for AMIs owned
%% by the user making the request.
%% 
%% The executable list may be provided to request information for AMIs that only
%% the specified users have launch permissions for. The items of the list may be
%% account ids for AMIs owned by the user making the request that the users with
%% the specified account ids have explicit launch permissions for, self for AMIs the
%% user making the request has explicit launch permissions for or all for public
%% AMIs.
%% 
%% Deregistered images will be included in the returned results for an unspecified
%% interval subsequent to deregistration. 

describe_images(Key, AccessKey,
		ImageId_n,
		Owner_n,
		ExecutableBy_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "DescribeImages"},
		create_ec2_param_list("ImageId", ImageId_n),
		create_ec2_param_list("Owner", Owner_n),
		create_ec2_param_list("ExecutableBy", ExecutableBy_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.

%% DescribeInstances
%%
%% The DescribeInstances operation returns information about instances owned by
%% the user making the request.
%% 
%% An optional list of instance IDs may be provided to request information for those
%% instances only. If no instance IDs are provided, information of all relevant
%% instances information will be returned. If an instance is specified that does not
%% exist a fault is returned. If an instance is specified that exists but is not owned
%% by the user making the request, then that instance will not be included in the
%% returned results.
%% 
%% Recently terminated instances will be included in the returned results for a small
%% interval subsequent to their termination. This interval is typically of the order of
%% one hour.
%% 

describe_instances(Key, AccessKey,
		InstanceId_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "DescribeInstances"},
		create_ec2_param_list("InstanceId", InstanceId_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DescribeKeyPairs
%%
%% The DescribeKeyPairs operation returns information about keypairs available
%% for use by the user making the request. Selected keypairs may be specified or
%% the list may be left empty if information for all registered keypairs is required.

describe_key_pairs(Key, AccessKey,
		KeyName_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "DescribeKeyPairs"},
		create_ec2_param_list("KeyName", KeyName_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% DescribeSecurityGroups
%%
%% The DescribeSecurityGroups operation returns information about security
%% groups owned by the user making the request.
%% 
%% An optional list of security group names may be provided to request information
%% for those security groups only. If no security group names are provided,
%% information of all security groups will be returned. If a group is specified that
%% does not exist a fault is returned.
%% 

describe_security_groups(Key, AccessKey,
		GroupName_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "DescribeSecurityGroups"},
		create_ec2_param_list("GroupName", GroupName_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% GetConsoleOutput
%%
%% The GetConsoleOutput operation retrieves console output that has been posted
%% for the specified instance.
%% 
%% Instance console output is buffered and posted shortly after instance boot,
%% reboot and once the instance is terminated. Only the most recent 64 KB of
%% posted output is available. Console output is available for at least 1 hour after
%% the most recent post. 

get_console_output(Key, AccessKey,
		InstanceId
	) ->
	Params = add_default_params(
		[{"Action", "GetConsoleOutput"},
		{"InstanceId", InstanceId}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% ModifyImageAttribute
%%
%% The ModifyImageAttribute operation modifies an attribute of an AMI.
%% 
%% Attribute Name	Type	Description
%% launchPermission	List	Controls who has permission to launch the AMI.
%% Launch permissions can be granted to specific users by adding userIds. The
%% AMI can be made public by adding the all group.
%% productCodes	List	Associates product codes with AMIs. This allows a
%% developer to charge a user extra for using the AMIs. productCodes is a write
%% once attribute - once it has been set it can not be changed or removed.
%% 
%% Attribute Name	Description	Supported Operations
%% launchPermission	Modifies the AMI's launch permissions.	add, remove
%% productCodes	Attaches a product code to the AMIs. The productCodes
%% attribute is a write once attribute.	operation not required

modify_image_attribute(Key, AccessKey,
		ImageId,
		Attribute,
		OperationType,
		UserId_n,
		UserGroup_n,
		ProductCode_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "ModifyImageAttribute"},
		{"ImageId", ImageId},
		{"Attribute", Attribute},
		{"OperationType", OperationType},
		create_ec2_param_list("UserId", UserId_n),
		create_ec2_param_list("UserGroup", UserGroup_n),
		create_ec2_param_list("ProductCode", ProductCode_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% RebootInstances
%%
%% The RebootInstances operation requests a reboot of one or more instances.
%% This operation is asynchronous; it only queues a request to reboot the specified
%% instance(s). The operation will succeed provided the instances are valid and
%% belong to the user. Terminated instances will be ignored.
%% Request Parameters

reboot_instances(Key, AccessKey,
		InstanceId_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "RebootInstances"},
		create_ec2_param_list("InstanceId", InstanceId_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% RegisterImage
%%
%% The RegisterImage operation registers an AMI with Amazon EC2. Images must
%% be registered before they can be launched.
%% 
%% Each AMI is associated with an unique ID which is provided by the EC2 service
%% through the Registerimage operation. As part of the registration process,
%% Amazon EC2 will retrieve the specified image manifest from Amazon S3 and
%% verify that the image is owned by the user requesting image registration.
%% 
%% The image manifest is retrieved once and stored within the Amazon EC2
%% network. Any modifications to an image in Amazon S3 invalidate this
%% registration. If you do have to make changes and upload a new image
%% deregister the previous image and register the new image. 

register_image(Key, AccessKey,
		ImageLocation
	) ->
	Params = add_default_params(
		[{"Action", "RegisterImage"},
		{"ImageLocation", ImageLocation}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% ResetImageAttribute
%%
%% The ResetImageAttribute operation resets an attribute of an AMI to its default
%% value.
%% 
%% The productCodes attribute cannot be reset. 

reset_image_attribute(Key, AccessKey,
		ImageId,
		Attribute
	) ->
	Params = add_default_params(
		[{"Action", "ResetImageAttribute"},
		{"ImageId", ImageId},
		{"Attribute", Attribute}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% RevokeSecurityGroupIngress
%%
%% The RevokeSecurityGroupIngress operation revokes existing permissions that
%% were previously granted to a security group. The permissions to revoke must be
%% specified using the same values originally used to grant the permission.
%% 
%% Permissions are specified in terms of the IP protocol (TCP, UDP or ICMP), the
%% source of the request (by IP range or an Amazon EC2 user-group pair), source
%% and destination port ranges (for TCP and UDP), and ICMP codes and types (for
%% ICMP). When authorizing ICMP, -1 may be used as a wildcard in the type and
%% code fields.
%% 
%% Permission changes are propagated to instances within the security group
%% being modified as quickly as possible. However, a small delay is likely,
%% depending on the number of instances that are members of the indicated group.
%% 
%% When revoking a user/group pair permission, GroupName,
%% SourceSecurityGroupName and SourceSecurityGroupOwnerId must be
%% specified. When authorizing a CIDR IP permission, GroupName, IpProtocol,
%% FromPort, ToPort and CidrIp must be specified. Mixing these two types of
%% parameters is not allowed. 

revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId
	) ->
	revoke_security_group_ingress(Key, AccessKey,
			GroupName,
			SourceSecurityGroupName,
			SourceSecurityGroupOwnerId,
			null,
			null,
			null,
			null).

revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	revoke_security_group_ingress(Key, AccessKey,
			GroupName,
			null,
			null,
			IpProtocol,
			FromPort,
			ToPort,
			CidrIp).

revoke_security_group_ingress(Key, AccessKey,
		GroupName,
		SourceSecurityGroupName,
		SourceSecurityGroupOwnerId,
		IpProtocol,
		FromPort,
		ToPort,
		CidrIp
	) ->
	Params = add_default_params(
		[{"Action", "RevokeSecurityGroupIngress"},
		{"GroupName", GroupName},
		{"SourceSecurityGroupName", SourceSecurityGroupName},
		{"SourceSecurityGroupOwnerId", SourceSecurityGroupOwnerId},
		{"IpProtocol", IpProtocol},
		{"FromPort", FromPort},
		{"ToPort", ToPort},
		{"CidrIp", CidrIp}],
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% RunInstances
%%
%% The RunInstances operation launches a specified number of instances.
%% 
%% Note: The Query version of RunInstances only allows instances of a single AMI
%% to be launched in one call. This is different from the SOAP API call of the same
%% name but similar to the ec2-run-instances command line tool.
%% 
%% A call to RunInstances is guaranteed to start no fewer than the requested
%% minimum. If there is insufficient capacity available then no instances will be
%% started. Amazon EC2 will make a best effort attempt to satisfy the requested
%% maximum values.
%% 
%% Every instance is launched in a security group. This may be specified as part of
%% the launch request. If a security group is not indicated then instances are
%% started in a the default security group.
%% 
%% An optional keypair ID may be provided for each image in the launch request.
%% All instances that are created from images for which this is provided will have
%% access to the associated public key at boot time (detailed below). This key may
%% be used to provide secure access to an instance of an image on a per-instance
%% basis. Amazon EC2 public images make use of this functionality to provide
%% secure passwordless access to instances (and launching those images without
%% a keypair ID will leave them inaccessible).
%% 
%% The public key material is made available to the instance at boot time by placing
%% it in a file named openssh_id.pub on a logical device that is exposed to the
%% instance as /dev/sda2 (the ephemeral store). The format of this file is suitable
%% for use as an entry within ~/.ssh/authorized_keys (the OpenSSH format). This
%% can be done at boot time (as part of rclocal, for example) allowing for secure
%% password-less access. As the need arises, other formats will also be
%% considered.
%% 
%% If the AMI has a product code attached for which the user has not subscribed,
%% the RunInstances call will fail. 

run_instances(Key, AccessKey,
		ImageId,
		MinCount,
		MaxCount,
		KeyName,
		SecurityGroup_n,
		UserData,
		AddressingType
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "RunInstances"},
		{"ImageId", ImageId},
		{"MinCount", integer_to_list(MinCount)},
		{"MaxCount", integer_to_list(MaxCount)},
		{"KeyName", KeyName},
		create_ec2_param_list("SecurityGroup", SecurityGroup_n),
		{"UserData", UserData},
		{"AddressingType", AddressingType}]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
%% TerminateInstances
%%
%% The TerminateInstances operation shuts down one or more instances. This
%% operation is idempotent and terminating an instance that is in the process of
%% shutting down (or already terminated) will succeed.
%% 
%% Terminated instances remain visible for a short period of time (approximately
%% one hour) after termination, after which their instance ID is invalidated. 

terminate_instances(Key, AccessKey,
		InstanceId_n
	) ->
	Params = add_default_params(
		lists:flatten([{"Action", "TerminateInstances"},
		create_ec2_param_list("InstanceId", InstanceId_n)]),
		AccessKey),
	Url = ec2_url(Key, Params),
	{ ok, {_Status, _Headers, Body }} = httpc:request(Url),
	Body.
		
