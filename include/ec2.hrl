%% HRL file generated by ERLSOM
%%
%% It is possible to change the name of the record fields.
%%
%% It is possible to add default values, but be aware that these will
%% only be used when *writing* an xml document.

-record('ec2:_document-UnmonitorInstancesResponse', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:_document-MonitorInstancesResponse', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:_document-UnmonitorInstances', {anyAttribs, 'instancesSet'}).
-record('ec2:_document-MonitorInstances', {anyAttribs, 'instancesSet'}).
-record('ec2:DeleteTagsResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteTagsType', {anyAttribs, 'resourcesSet', 'tagSet'}).
-record('ec2:DeleteTagsSetType', {anyAttribs, 'item'}).
-record('ec2:DeleteTagsSetItemType', {anyAttribs, 'key', 'value'}).
-record('ec2:DescribeTagsResponseType', {anyAttribs, 'requestId', 'tagSet'}).
-record('ec2:DescribeTagsType', {anyAttribs, 'filterSet'}).
-record('ec2:TagSetType', {anyAttribs, 'item'}).
-record('ec2:TagSetItemType', {anyAttribs, 'resourceId', 'resourceType', 'key', 'value'}).
-record('ec2:CreateTagsResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:CreateTagsType', {anyAttribs, 'resourcesSet', 'tagSet'}).
-record('ec2:ResourceTagSetType', {anyAttribs, 'item'}).
-record('ec2:ResourceTagSetItemType', {anyAttribs, 'key', 'value'}).
-record('ec2:ResourceIdSetItemType', {anyAttribs, 'resourceId'}).
-record('ec2:ResourceIdSetType', {anyAttribs, 'item'}).
-record('ec2:DescribePlacementGroupsResponseType', {anyAttribs, 'requestId', 'placementGroupSet'}).
-record('ec2:PlacementGroupSetType', {anyAttribs, 'item'}).
-record('ec2:PlacementGroupInfoType', {anyAttribs, 'groupName', 'strategy', 'state'}).
-record('ec2:DescribePlacementGroupsType', {anyAttribs, 'placementGroupSet', 'filterSet'}).
-record('ec2:DescribePlacementGroupsInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribePlacementGroupItemType', {anyAttribs, 'groupName'}).
-record('ec2:DeletePlacementGroupResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeletePlacementGroupType', {anyAttribs, 'groupName'}).
-record('ec2:CreatePlacementGroupResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:CreatePlacementGroupType', {anyAttribs, 'groupName', 'strategy'}).
-record('ec2:DeactivateLicenseResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeactivateLicenseType', {anyAttribs, 'licenseId', 'capacity'}).
-record('ec2:ActivateLicenseResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ActivateLicenseType', {anyAttribs, 'licenseId', 'capacity'}).
-record('ec2:LicenseCapacitySetItemType', {anyAttribs, 'capacity', 'instanceCapacity', 'state', 'earliestAllowedDeactivationTime'}).
-record('ec2:LicenseCapacitySetType', {anyAttribs, 'item'}).
-record('ec2:LicenseSetItemType', {anyAttribs, 'licenseId', 'type', 'pool', 'capacitySet', 'tagSet'}).
-record('ec2:LicenseSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeLicensesResponseType', {anyAttribs, 'requestId', 'licenseSet'}).
-record('ec2:LicenseIdSetItemType', {anyAttribs, 'licenseId'}).
-record('ec2:LicenseIdSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeLicensesType', {anyAttribs, 'licenseIdSet', 'filterSet'}).
-record('ec2:DeleteSpotDatafeedSubscriptionResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteSpotDatafeedSubscriptionType', {anyAttribs}).
-record('ec2:DescribeSpotDatafeedSubscriptionResponseType', {anyAttribs, 'requestId', 'spotDatafeedSubscription'}).
-record('ec2:DescribeSpotDatafeedSubscriptionType', {anyAttribs}).
-record('ec2:CreateSpotDatafeedSubscriptionResponseType', {anyAttribs, 'requestId', 'spotDatafeedSubscription'}).
-record('ec2:CreateSpotDatafeedSubscriptionType', {anyAttribs, 'bucket', 'prefix'}).
-record('ec2:SpotDatafeedSubscriptionType', {anyAttribs, 'ownerId', 'bucket', 'prefix', 'state', 'fault'}).
-record('ec2:SpotPriceHistorySetItemType', {anyAttribs, 'instanceType', 'productDescription', 'spotPrice', 'timestamp'}).
-record('ec2:SpotPriceHistorySetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSpotPriceHistoryResponseType', {anyAttribs, 'requestId', 'spotPriceHistorySet'}).
-record('ec2:ProductDescriptionSetItemType', {anyAttribs, 'productDescription'}).
-record('ec2:ProductDescriptionSetType', {anyAttribs, 'item'}).
-record('ec2:InstanceTypeSetItemType', {anyAttribs, 'instanceType'}).
-record('ec2:InstanceTypeSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSpotPriceHistoryType', {anyAttribs, 'startTime', 'endTime', 'instanceTypeSet', 'productDescriptionSet', 'filterSet'}).
-record('ec2:CancelSpotInstanceRequestsResponseSetItemType', {anyAttribs, 'spotInstanceRequestId', 'state'}).
-record('ec2:CancelSpotInstanceRequestsResponseSetType', {anyAttribs, 'item'}).
-record('ec2:CancelSpotInstanceRequestsResponseType', {anyAttribs, 'requestId', 'spotInstanceRequestSet'}).
-record('ec2:CancelSpotInstanceRequestsType', {anyAttribs, 'spotInstanceRequestIdSet'}).
-record('ec2:DescribeSpotInstanceRequestsResponseType', {anyAttribs, 'requestId', 'spotInstanceRequestSet'}).
-record('ec2:SpotInstanceRequestIdSetItemType', {anyAttribs, 'spotInstanceRequestId'}).
-record('ec2:SpotInstanceRequestIdSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSpotInstanceRequestsType', {anyAttribs, 'spotInstanceRequestIdSet', 'filterSet'}).
-record('ec2:RequestSpotInstancesResponseType', {anyAttribs, 'requestId', 'spotInstanceRequestSet'}).
-record('ec2:SpotInstanceRequestSetType', {anyAttribs, 'item'}).
-record('ec2:SpotInstanceStateFaultType', {anyAttribs, 'code', 'message'}).
-record('ec2:SpotInstanceRequestSetItemType', {anyAttribs, 'spotInstanceRequestId', 'spotPrice', 'type', 'state', 'fault', 'validFrom', 'validUntil', 'launchGroup', 'availabilityZoneGroup', 'launchSpecification', 'instanceId', 'createTime', 'productDescription', 'tagSet'}).
-record('ec2:LaunchSpecificationResponseType', {anyAttribs, 'imageId', 'keyName', 'groupSet', 'addressingType', 'instanceType', 'placement', 'kernelId', 'ramdiskId', 'blockDeviceMapping', 'monitoring', 'subnetId'}).
-record('ec2:LaunchSpecificationRequestType', {anyAttribs, 'imageId', 'keyName', 'groupSet', 'userData', 'addressingType', 'instanceType', 'placement', 'kernelId', 'ramdiskId', 'blockDeviceMapping', 'monitoring', 'subnetId'}).
-record('ec2:RequestSpotInstancesType', {anyAttribs, 'spotPrice', 'instanceCount', 'type', 'validFrom', 'validUntil', 'launchGroup', 'availabilityZoneGroup', 'launchSpecification'}).
-record('ec2:AssociateDhcpOptionsResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:AssociateDhcpOptionsType', {anyAttribs, 'dhcpOptionsId', 'vpcId'}).
-record('ec2:CreateDhcpOptionsResponseType', {anyAttribs, 'requestId', 'dhcpOptions'}).
-record('ec2:CreateDhcpOptionsType', {anyAttribs, 'dhcpConfigurationSet'}).
-record('ec2:DescribeDhcpOptionsResponseType', {anyAttribs, 'requestId', 'dhcpOptionsSet'}).
-record('ec2:DescribeDhcpOptionsType', {anyAttribs, 'dhcpOptionsSet', 'filterSet'}).
-record('ec2:DeleteDhcpOptionsResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteDhcpOptionsType', {anyAttribs, 'dhcpOptionsId'}).
-record('ec2:DeleteSubnetResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteSubnetType', {anyAttribs, 'subnetId'}).
-record('ec2:DescribeSubnetsResponseType', {anyAttribs, 'requestId', 'subnetSet'}).
-record('ec2:DescribeSubnetsType', {anyAttribs, 'subnetSet', 'filterSet'}).
-record('ec2:CreateSubnetResponseType', {anyAttribs, 'requestId', 'subnet'}).
-record('ec2:CreateSubnetType', {anyAttribs, 'vpcId', 'cidrBlock', 'availabilityZone'}).
-record('ec2:DeleteVpcResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteVpcType', {anyAttribs, 'vpcId'}).
-record('ec2:DescribeVpcsResponseType', {anyAttribs, 'requestId', 'vpcSet'}).
-record('ec2:DescribeVpcsType', {anyAttribs, 'vpcSet', 'filterSet'}).
-record('ec2:CreateVpcResponseType', {anyAttribs, 'requestId', 'vpc'}).
-record('ec2:CreateVpcType', {anyAttribs, 'cidrBlock'}).
-record('ec2:DetachVpnGatewayResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DetachVpnGatewayType', {anyAttribs, 'vpnGatewayId', 'vpcId'}).
-record('ec2:AttachVpnGatewayResponseType', {anyAttribs, 'requestId', 'attachment'}).
-record('ec2:AttachVpnGatewayType', {anyAttribs, 'vpnGatewayId', 'vpcId'}).
-record('ec2:DescribeVpnConnectionsResponseType', {anyAttribs, 'requestId', 'vpnConnectionSet'}).
-record('ec2:DescribeVpnConnectionsType', {anyAttribs, 'vpnConnectionSet', 'filterSet'}).
-record('ec2:DeleteVpnConnectionResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteVpnConnectionType', {anyAttribs, 'vpnConnectionId'}).
-record('ec2:CreateVpnConnectionResponseType', {anyAttribs, 'requestId', 'vpnConnection'}).
-record('ec2:CreateVpnConnectionType', {anyAttribs, 'type', 'customerGatewayId', 'vpnGatewayId'}).
-record('ec2:DescribeVpnGatewaysResponseType', {anyAttribs, 'requestId', 'vpnGatewaySet'}).
-record('ec2:DescribeVpnGatewaysType', {anyAttribs, 'vpnGatewaySet', 'filterSet'}).
-record('ec2:DeleteVpnGatewayResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteVpnGatewayType', {anyAttribs, 'vpnGatewayId'}).
-record('ec2:CreateVpnGatewayResponseType', {anyAttribs, 'requestId', 'vpnGateway'}).
-record('ec2:CreateVpnGatewayType', {anyAttribs, 'type', 'availabilityZone'}).
-record('ec2:DescribeCustomerGatewaysResponseType', {anyAttribs, 'requestId', 'customerGatewaySet'}).
-record('ec2:DescribeCustomerGatewaysType', {anyAttribs, 'customerGatewaySet', 'filterSet'}).
-record('ec2:DeleteCustomerGatewayResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteCustomerGatewayType', {anyAttribs, 'customerGatewayId'}).
-record('ec2:CreateCustomerGatewayResponseType', {anyAttribs, 'requestId', 'customerGateway'}).
-record('ec2:CreateCustomerGatewayType', {anyAttribs, 'type', 'ipAddress', 'bgpAsn'}).
-record('ec2:ValueSetType', {anyAttribs, 'item'}).
-record('ec2:ValueType', {anyAttribs, 'value'}).
-record('ec2:FilterSetType', {anyAttribs, 'item'}).
-record('ec2:FilterType', {anyAttribs, 'name', 'valueSet'}).
-record('ec2:DhcpValueSetType', {anyAttribs, 'item'}).
-record('ec2:DhcpValueType', {anyAttribs, 'value'}).
-record('ec2:DhcpOptionsType', {anyAttribs, 'dhcpOptionsId', 'dhcpConfigurationSet', 'tagSet'}).
-record('ec2:DhcpConfigurationItemType', {anyAttribs, 'key', 'valueSet'}).
-record('ec2:DhcpOptionsSetType', {anyAttribs, 'item'}).
-record('ec2:DhcpConfigurationItemSetType', {anyAttribs, 'item'}).
-record('ec2:DhcpOptionsIdSetType', {anyAttribs, 'item'}).
-record('ec2:DhcpOptionsIdSetItemType', {anyAttribs, 'dhcpOptionsId'}).
-record('ec2:SubnetIdSetType', {anyAttribs, 'item'}).
-record('ec2:SubnetIdSetItemType', {anyAttribs, 'subnetId'}).
-record('ec2:VpcIdSetType', {anyAttribs, 'item'}).
-record('ec2:VpcIdSetItemType', {anyAttribs, 'vpcId'}).
-record('ec2:VpnConnectionIdSetType', {anyAttribs, 'item'}).
-record('ec2:VpnConnectionIdSetItemType', {anyAttribs, 'vpnConnectionId'}).
-record('ec2:VpnGatewayIdSetType', {anyAttribs, 'item'}).
-record('ec2:VpnGatewayIdSetItemType', {anyAttribs, 'vpnGatewayId'}).
-record('ec2:CustomerGatewayIdSetType', {anyAttribs, 'item'}).
-record('ec2:CustomerGatewayIdSetItemType', {anyAttribs, 'customerGatewayId'}).
-record('ec2:SubnetSetType', {anyAttribs, 'item'}).
-record('ec2:VpcSetType', {anyAttribs, 'item'}).
-record('ec2:VpnConnectionSetType', {anyAttribs, 'item'}).
-record('ec2:VpnGatewaySetType', {anyAttribs, 'item'}).
-record('ec2:CustomerGatewaySetType', {anyAttribs, 'item'}).
-record('ec2:SubnetType', {anyAttribs, 'subnetId', 'state', 'vpcId', 'cidrBlock', 'availableIpAddressCount', 'availabilityZone', 'tagSet'}).
-record('ec2:VpcType', {anyAttribs, 'vpcId', 'state', 'cidrBlock', 'dhcpOptionsId', 'tagSet'}).
-record('ec2:VpnConnectionType', {anyAttribs, 'vpnConnectionId', 'state', 'customerGatewayConfiguration', 'type', 'customerGatewayId', 'vpnGatewayId', 'tagSet'}).
-record('ec2:CustomerGatewayType', {anyAttribs, 'customerGatewayId', 'state', 'type', 'ipAddress', 'bgpAsn', 'tagSet'}).
-record('ec2:VpnGatewayType', {anyAttribs, 'vpnGatewayId', 'state', 'type', 'availabilityZone', 'attachments', 'tagSet'}).
-record('ec2:AttachmentSetType', {anyAttribs, 'item'}).
-record('ec2:AttachmentType', {anyAttribs, 'vpcId', 'state'}).
-record('ec2:InstanceMonitoringStateType', {anyAttribs, 'state'}).
-record('ec2:MonitorInstancesResponseSetItemType', {anyAttribs, 'instanceId', 'monitoring'}).
-record('ec2:MonitorInstancesResponseSetType', {anyAttribs, 'item'}).
-record('ec2:MonitorInstancesResponseType', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:MonitorInstancesSetItemType', {anyAttribs, 'instanceId'}).
-record('ec2:MonitorInstancesSetType', {anyAttribs, 'item'}).
-record('ec2:MonitorInstancesType', {anyAttribs, 'instancesSet'}).
-record('ec2:DescribeReservedInstancesResponseSetItemType', {anyAttribs, 'reservedInstancesId', 'instanceType', 'availabilityZone', 'start', 'duration', 'fixedPrice', 'usagePrice', 'instanceCount', 'productDescription', 'state', 'tagSet'}).
-record('ec2:DescribeReservedInstancesResponseSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeReservedInstancesResponseType', {anyAttribs, 'requestId', 'reservedInstancesSet'}).
-record('ec2:DescribeReservedInstancesSetItemType', {anyAttribs, 'reservedInstancesId'}).
-record('ec2:DescribeReservedInstancesSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeReservedInstancesType', {anyAttribs, 'reservedInstancesSet', 'filterSet'}).
-record('ec2:PurchaseReservedInstancesOfferingResponseType', {anyAttribs, 'requestId', 'reservedInstancesId'}).
-record('ec2:PurchaseReservedInstancesOfferingType', {anyAttribs, 'reservedInstancesOfferingId', 'instanceCount'}).
-record('ec2:DescribeReservedInstancesOfferingsResponseSetItemType', {anyAttribs, 'reservedInstancesOfferingId', 'instanceType', 'availabilityZone', 'duration', 'fixedPrice', 'usagePrice', 'productDescription'}).
-record('ec2:DescribeReservedInstancesOfferingsResponseSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeReservedInstancesOfferingsResponseType', {anyAttribs, 'requestId', 'reservedInstancesOfferingsSet'}).
-record('ec2:DescribeReservedInstancesOfferingsSetItemType', {anyAttribs, 'reservedInstancesOfferingId'}).
-record('ec2:DescribeReservedInstancesOfferingsSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeReservedInstancesOfferingsType', {anyAttribs, 'reservedInstancesOfferingsSet', 'instanceType', 'availabilityZone', 'productDescription', 'filterSet'}).
-record('ec2:RegionItemType', {anyAttribs, 'regionName', 'regionEndpoint'}).
-record('ec2:RegionSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeRegionsResponseType', {anyAttribs, 'requestId', 'regionInfo'}).
-record('ec2:DescribeRegionsSetItemType', {anyAttribs, 'regionName'}).
-record('ec2:DescribeRegionsSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeRegionsType', {anyAttribs, 'regionSet', 'filterSet'}).
-record('ec2:CancelBundleTaskResponseType', {anyAttribs, 'requestId', 'bundleInstanceTask'}).
-record('ec2:CancelBundleTaskType', {anyAttribs, 'bundleId'}).
-record('ec2:BundleInstanceTasksSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeBundleTasksResponseType', {anyAttribs, 'requestId', 'bundleInstanceTasksSet'}).
-record('ec2:DescribeBundleTasksItemType', {anyAttribs, 'bundleId'}).
-record('ec2:DescribeBundleTasksInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeBundleTasksType', {anyAttribs, 'bundlesSet', 'filterSet'}).
-record('ec2:BundleInstanceTaskErrorType', {anyAttribs, 'code', 'message'}).
-record('ec2:BundleInstanceTaskType', {anyAttribs, 'instanceId', 'bundleId', 'state', 'startTime', 'updateTime', 'storage', 'progress', 'error'}).
-record('ec2:BundleInstanceResponseType', {anyAttribs, 'requestId', 'bundleInstanceTask'}).
-record('ec2:BundleInstanceS3StorageType', {anyAttribs, 'bucket', 'prefix', 'awsAccessKeyId', 'uploadPolicy', 'uploadPolicySignature'}).
-record('ec2:BundleInstanceTaskStorageType', {anyAttribs, 'S3'}).
-record('ec2:BundleInstanceType', {anyAttribs, 'instanceId', 'storage'}).
-record('ec2:DescribeSnapshotAttributeResponseType', {anyAttribs, 'requestId', 'snapshotId', 'createVolumePermission'}).
-record('ec2:DescribeSnapshotAttributesGroup', {anyAttribs, 'createVolumePermission'}).
-record('ec2:DescribeSnapshotAttributeType', {anyAttribs, 'snapshotId', 'DescribeSnapshotAttributesGroup'}).
-record('ec2:ResetSnapshotAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ResetSnapshotAttributesGroup', {anyAttribs, 'createVolumePermission'}).
-record('ec2:ResetSnapshotAttributeType', {anyAttribs, 'snapshotId', 'ResetSnapshotAttributesGroup'}).
-record('ec2:ModifySnapshotAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:CreateVolumePermissionItemType-userId', {anyAttribs, 'userId'}).
-record('ec2:CreateVolumePermissionItemType-group', {anyAttribs, 'group'}).
-record('ec2:CreateVolumePermissionItemType', {anyAttribs, choice}).
-record('ec2:CreateVolumePermissionListType', {anyAttribs, 'item'}).
-record('ec2:CreateVolumePermissionOperationType-add', {anyAttribs, 'item'}).
-record('ec2:CreateVolumePermissionOperationType-remove', {anyAttribs, 'item'}).
-record('ec2:CreateVolumePermissionOperationType', {anyAttribs, choice}).
-record('ec2:ModifySnapshotAttributeType', {anyAttribs, 'snapshotId', 'createVolumePermission'}).
-record('ec2:DescribeSnapshotsSetItemResponseType', {anyAttribs, 'snapshotId', 'volumeId', 'status', 'startTime', 'progress', 'ownerId', 'volumeSize', 'description', 'ownerAlias', 'tagSet'}).
-record('ec2:DescribeSnapshotsSetResponseType', {anyAttribs, 'item'}).
-record('ec2:DescribeSnapshotsResponseType', {anyAttribs, 'requestId', 'snapshotSet'}).
-record('ec2:DescribeSnapshotsRestorableByType', {anyAttribs, 'user'}).
-record('ec2:DescribeSnapshotsRestorableBySetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSnapshotsOwnerType', {anyAttribs, 'owner'}).
-record('ec2:DescribeSnapshotsOwnersType', {anyAttribs, 'item'}).
-record('ec2:DescribeSnapshotsSetItemType', {anyAttribs, 'snapshotId'}).
-record('ec2:DescribeSnapshotsSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSnapshotsType', {anyAttribs, 'snapshotSet', 'ownersSet', 'restorableBySet', 'filterSet'}).
-record('ec2:DeleteSnapshotResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteSnapshotType', {anyAttribs, 'snapshotId'}).
-record('ec2:CreateSnapshotResponseType', {anyAttribs, 'requestId', 'snapshotId', 'volumeId', 'status', 'startTime', 'progress', 'ownerId', 'volumeSize', 'description'}).
-record('ec2:CreateSnapshotType', {anyAttribs, 'volumeId', 'description'}).
-record('ec2:DetachVolumeResponseType', {anyAttribs, 'requestId', 'volumeId', 'instanceId', 'device', 'status', 'attachTime'}).
-record('ec2:DetachVolumeType', {anyAttribs, 'volumeId', 'instanceId', 'device', 'force'}).
-record('ec2:AttachVolumeResponseType', {anyAttribs, 'requestId', 'volumeId', 'instanceId', 'device', 'status', 'attachTime'}).
-record('ec2:AttachVolumeType', {anyAttribs, 'volumeId', 'instanceId', 'device'}).
-record('ec2:AttachmentSetItemResponseType', {anyAttribs, 'volumeId', 'instanceId', 'device', 'status', 'attachTime', 'deleteOnTermination'}).
-record('ec2:AttachmentSetResponseType', {anyAttribs, 'item'}).
-record('ec2:DescribeVolumesSetItemResponseType', {anyAttribs, 'volumeId', 'size', 'snapshotId', 'availabilityZone', 'status', 'createTime', 'attachmentSet', 'tagSet'}).
-record('ec2:DescribeVolumesSetResponseType', {anyAttribs, 'item'}).
-record('ec2:DescribeVolumesResponseType', {anyAttribs, 'requestId', 'volumeSet'}).
-record('ec2:DescribeVolumesSetItemType', {anyAttribs, 'volumeId'}).
-record('ec2:DescribeVolumesSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeVolumesType', {anyAttribs, 'volumeSet', 'filterSet'}).
-record('ec2:DeleteVolumeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteVolumeType', {anyAttribs, 'volumeId'}).
-record('ec2:CreateVolumeResponseType', {anyAttribs, 'requestId', 'volumeId', 'size', 'snapshotId', 'availabilityZone', 'status', 'createTime'}).
-record('ec2:CreateVolumeType', {anyAttribs, 'size', 'snapshotId', 'availabilityZone'}).
-record('ec2:DisassociateAddressResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DisassociateAddressType', {anyAttribs, 'publicIp'}).
-record('ec2:AssociateAddressResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:AssociateAddressType', {anyAttribs, 'publicIp', 'instanceId'}).
-record('ec2:DescribeAddressesResponseItemType', {anyAttribs, 'publicIp', 'instanceId'}).
-record('ec2:DescribeAddressesResponseInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeAddressesResponseType', {anyAttribs, 'requestId', 'addressesSet'}).
-record('ec2:DescribeAddressesItemType', {anyAttribs, 'publicIp'}).
-record('ec2:DescribeAddressesInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeAddressesType', {anyAttribs, 'publicIpsSet', 'filterSet'}).
-record('ec2:ReleaseAddressResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ReleaseAddressType', {anyAttribs, 'publicIp'}).
-record('ec2:AllocateAddressResponseType', {anyAttribs, 'requestId', 'publicIp'}).
-record('ec2:AllocateAddressType', {anyAttribs}).
-record('ec2:AvailabilityZoneItemType', {anyAttribs, 'zoneName', 'zoneState', 'regionName', 'messageSet'}).
-record('ec2:AvailabilityZoneMessageSetType', {anyAttribs, 'item'}).
-record('ec2:AvailabilityZoneMessageType', {anyAttribs, 'message'}).
-record('ec2:AvailabilityZoneSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeAvailabilityZonesResponseType', {anyAttribs, 'requestId', 'availabilityZoneInfo'}).
-record('ec2:DescribeAvailabilityZonesSetItemType', {anyAttribs, 'zoneName'}).
-record('ec2:DescribeAvailabilityZonesSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeAvailabilityZonesType', {anyAttribs, 'availabilityZoneSet', 'filterSet'}).
-record('ec2:ConfirmProductInstanceResponseType', {anyAttribs, 'requestId', 'return', 'ownerId'}).
-record('ec2:ProductCodesSetItemType', {anyAttribs, 'productCode'}).
-record('ec2:ProductCodesSetType', {anyAttribs, 'item'}).
-record('ec2:ConfirmProductInstanceType', {anyAttribs, 'productCode', 'instanceId'}).
-record('ec2:AttributeBooleanValueType', {anyAttribs, 'value'}).
-record('ec2:AttributeValueType', {anyAttribs, 'value'}).
-record('ec2:NullableAttributeBooleanValueType', {anyAttribs, 'value'}).
-record('ec2:NullableAttributeValueType', {anyAttribs, 'value'}).
-record('ec2:DescribeImageAttributeResponseType-kernel', {anyAttribs, 'value'}).
-record('ec2:DescribeImageAttributeResponseType-ramdisk', {anyAttribs, 'value'}).
-record('ec2:DescribeImageAttributeResponseType-description', {anyAttribs, 'value'}).
-record('ec2:DescribeImageAttributeResponseType', {anyAttribs, 'requestId', 'imageId', choice}).
-record('ec2:DescribeImageAttributesGroup-launchPermission', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup-productCodes', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup-kernel', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup-ramdisk', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup-blockDeviceMapping', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup-description', {anyAttribs}).
-record('ec2:DescribeImageAttributesGroup', {anyAttribs, choice}).
-record('ec2:DescribeImageAttributeType', {anyAttribs, 'imageId', choice}).
-record('ec2:ResetImageAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:EmptyElementType', {anyAttribs}).
-record('ec2:ResetImageAttributesGroup', {anyAttribs, 'launchPermission'}).
-record('ec2:ResetImageAttributeType', {anyAttribs, 'imageId', 'ResetImageAttributesGroup'}).
-record('ec2:ModifyImageAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ProductCodeItemType', {anyAttribs, 'productCode'}).
-record('ec2:ProductCodeListType', {anyAttribs, 'item'}).
-record('ec2:LaunchPermissionItemType-userId', {anyAttribs, 'userId'}).
-record('ec2:LaunchPermissionItemType-group', {anyAttribs, 'group'}).
-record('ec2:LaunchPermissionItemType', {anyAttribs, choice}).
-record('ec2:LaunchPermissionListType', {anyAttribs, 'item'}).
-record('ec2:LaunchPermissionOperationType-add', {anyAttribs, 'item'}).
-record('ec2:LaunchPermissionOperationType-remove', {anyAttribs, 'item'}).
-record('ec2:LaunchPermissionOperationType', {anyAttribs, choice}).
-record('ec2:ModifyImageAttributeType', {anyAttribs, 'imageId', choice}).
-record('ec2:DescribeInstanceAttributeResponseType-instanceType', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType-kernel', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType-ramdisk', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType-userData', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType-instanceInitiatedShutdownBehavior', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType-rootDeviceName', {anyAttribs, 'value'}).
-record('ec2:DescribeInstanceAttributeResponseType', {anyAttribs, 'requestId', 'instanceId', choice}).
-record('ec2:DescribeInstanceAttributesGroup-instanceType', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-kernel', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-ramdisk', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-userData', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-disableApiTermination', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-instanceInitiatedShutdownBehavior', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-rootDeviceName', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup-blockDeviceMapping', {anyAttribs}).
-record('ec2:DescribeInstanceAttributesGroup', {anyAttribs, choice}).
-record('ec2:DescribeInstanceAttributeType', {anyAttribs, 'instanceId', choice}).
-record('ec2:ResetInstanceAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ResetInstanceAttributesGroup-kernel', {anyAttribs}).
-record('ec2:ResetInstanceAttributesGroup-ramdisk', {anyAttribs}).
-record('ec2:ResetInstanceAttributesGroup', {anyAttribs, choice}).
-record('ec2:ResetInstanceAttributeType', {anyAttribs, 'instanceId', choice}).
-record('ec2:ModifyInstanceAttributeResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:ModifyInstanceAttributeType-instanceType', {anyAttribs, 'value'}).
-record('ec2:ModifyInstanceAttributeType-kernel', {anyAttribs, 'value'}).
-record('ec2:ModifyInstanceAttributeType-ramdisk', {anyAttribs, 'value'}).
-record('ec2:ModifyInstanceAttributeType-userData', {anyAttribs, 'value'}).
-record('ec2:ModifyInstanceAttributeType-instanceInitiatedShutdownBehavior', {anyAttribs, 'value'}).
-record('ec2:ModifyInstanceAttributeType', {anyAttribs, 'instanceId', choice}).
-record('ec2:InstanceStateType', {anyAttribs, 'code', 'name'}).
-record('ec2:RevokeSecurityGroupIngressResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:RevokeSecurityGroupIngressType', {anyAttribs, 'userId', 'groupName', 'ipPermissions'}).
-record('ec2:AuthorizeSecurityGroupIngressResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:AuthorizeSecurityGroupIngressType', {anyAttribs, 'userId', 'groupName', 'ipPermissions'}).
-record('ec2:SecurityGroupItemType', {anyAttribs, 'ownerId', 'groupName', 'groupDescription', 'ipPermissions'}).
-record('ec2:SecurityGroupSetType', {anyAttribs, 'item'}).
-record('ec2:UserIdGroupPairType', {anyAttribs, 'userId', 'groupName'}).
-record('ec2:UserIdGroupPairSetType', {anyAttribs, 'item'}).
-record('ec2:IpRangeItemType', {anyAttribs, 'cidrIp'}).
-record('ec2:IpRangeSetType', {anyAttribs, 'item'}).
-record('ec2:IpPermissionType', {anyAttribs, 'ipProtocol', 'fromPort', 'toPort', 'groups', 'ipRanges'}).
-record('ec2:IpPermissionSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSecurityGroupsResponseType', {anyAttribs, 'requestId', 'securityGroupInfo'}).
-record('ec2:DescribeSecurityGroupsSetItemType', {anyAttribs, 'groupName'}).
-record('ec2:DescribeSecurityGroupsSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeSecurityGroupsType', {anyAttribs, 'securityGroupSet', 'filterSet'}).
-record('ec2:DeleteSecurityGroupResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteSecurityGroupType', {anyAttribs, 'groupName'}).
-record('ec2:CreateSecurityGroupResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:CreateSecurityGroupType', {anyAttribs, 'groupName', 'groupDescription'}).
-record('ec2:DescribeImagesResponseItemType', {anyAttribs, 'imageId', 'imageLocation', 'imageState', 'imageOwnerId', 'isPublic', 'productCodes', 'architecture', 'imageType', 'kernelId', 'ramdiskId', 'platform', 'stateReason', 'imageOwnerAlias', 'name', 'description', 'rootDeviceType', 'rootDeviceName', 'blockDeviceMapping', 'virtualizationType', 'tagSet'}).
-record('ec2:DescribeImagesResponseInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeImagesResponseType', {anyAttribs, 'requestId', 'imagesSet'}).
-record('ec2:DescribeImagesExecutableByType', {anyAttribs, 'user'}).
-record('ec2:DescribeImagesExecutableBySetType', {anyAttribs, 'item'}).
-record('ec2:DescribeImagesOwnerType', {anyAttribs, 'owner'}).
-record('ec2:DescribeImagesOwnersType', {anyAttribs, 'item'}).
-record('ec2:DescribeImagesItemType', {anyAttribs, 'imageId'}).
-record('ec2:DescribeImagesInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeImagesType', {anyAttribs, 'executableBySet', 'imagesSet', 'ownersSet', 'filterSet'}).
-record('ec2:ReservationSetType', {anyAttribs, 'item'}).
-record('ec2:DescribeInstancesResponseType', {anyAttribs, 'requestId', 'reservationSet'}).
-record('ec2:DescribeInstancesItemType', {anyAttribs, 'instanceId'}).
-record('ec2:DescribeInstancesInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeInstancesType', {anyAttribs, 'instancesSet', 'filterSet'}).
-record('ec2:RebootInstancesResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:RebootInstancesItemType', {anyAttribs, 'instanceId'}).
-record('ec2:RebootInstancesInfoType', {anyAttribs, 'item'}).
-record('ec2:RebootInstancesType', {anyAttribs, 'instancesSet'}).
-record('ec2:StartInstancesResponseType', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:StartInstancesType', {anyAttribs, 'instancesSet'}).
-record('ec2:StopInstancesResponseType', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:StopInstancesType', {anyAttribs, 'instancesSet', 'force'}).
-record('ec2:InstanceEbsBlockDeviceType', {anyAttribs, 'volumeId', 'deleteOnTermination'}).
-record('ec2:InstanceBlockDeviceMappingItemType-virtualName', {anyAttribs, 'virtualName'}).
-record('ec2:InstanceBlockDeviceMappingItemType', {anyAttribs, 'deviceName', choice}).
-record('ec2:InstanceBlockDeviceMappingType', {anyAttribs, 'item'}).
-record('ec2:TerminateInstancesResponseType', {anyAttribs, 'requestId', 'instancesSet'}).
-record('ec2:TerminateInstancesType', {anyAttribs, 'instancesSet'}).
-record('ec2:InstanceStateChangeSetType', {anyAttribs, 'item'}).
-record('ec2:InstanceStateChangeType', {anyAttribs, 'instanceId', 'currentState', 'previousState'}).
-record('ec2:InstanceIdSetType', {anyAttribs, 'item'}).
-record('ec2:InstanceIdType', {anyAttribs, 'instanceId'}).
-record('ec2:GetPasswordDataResponseType', {anyAttribs, 'requestId', 'instanceId', 'timestamp', 'passwordData'}).
-record('ec2:GetPasswordDataType', {anyAttribs, 'instanceId'}).
-record('ec2:GetConsoleOutputResponseType', {anyAttribs, 'requestId', 'instanceId', 'timestamp', 'output'}).
-record('ec2:GetConsoleOutputType', {anyAttribs, 'instanceId'}).
-record('ec2:InstanceLicenseResponseType', {anyAttribs, 'pool'}).
-record('ec2:EbsInstanceBlockDeviceMappingResponseType', {anyAttribs, 'volumeId', 'status', 'attachTime', 'deleteOnTermination'}).
-record('ec2:InstanceBlockDeviceMappingResponseItemType', {anyAttribs, 'deviceName', 'ebs'}).
-record('ec2:InstanceBlockDeviceMappingResponseType', {anyAttribs, 'item'}).
-record('ec2:StateReasonType', {anyAttribs, 'code', 'message'}).
-record('ec2:PlacementResponseType', {anyAttribs, 'availabilityZone', 'groupName'}).
-record('ec2:RunningInstancesItemType', {anyAttribs, 'instanceId', 'imageId', 'instanceState', 'privateDnsName', 'dnsName', 'reason', 'keyName', 'amiLaunchIndex', 'productCodes', 'instanceType', 'launchTime', 'placement', 'kernelId', 'ramdiskId', 'platform', 'monitoring', 'subnetId', 'vpcId', 'privateIpAddress', 'ipAddress', 'stateReason', 'architecture', 'rootDeviceType', 'rootDeviceName', 'blockDeviceMapping', 'instanceLifecycle', 'spotInstanceRequestId', 'license', 'virtualizationType', 'clientToken', 'tagSet'}).
-record('ec2:RunningInstancesSetType', {anyAttribs, 'item'}).
-record('ec2:ReservationInfoType', {anyAttribs, 'reservationId', 'ownerId', 'groupSet', 'instancesSet', 'requesterId'}).
-record('ec2:RunInstancesResponseType', {anyAttribs, 'requestId', 'reservationId', 'ownerId', 'groupSet', 'instancesSet', 'requesterId'}).
-record('ec2:InstanceLicenseRequestType', {anyAttribs, 'pool'}).
-record('ec2:MonitoringInstanceType', {anyAttribs, 'enabled'}).
-record('ec2:PlacementRequestType', {anyAttribs, 'availabilityZone', 'groupName'}).
-record('ec2:EbsBlockDeviceType', {anyAttribs, 'snapshotId', 'volumeSize', 'deleteOnTermination'}).
-record('ec2:BlockDeviceMappingItemType-virtualName', {anyAttribs, 'virtualName'}).
-record('ec2:BlockDeviceMappingItemType', {anyAttribs, 'deviceName', choice}).
-record('ec2:BlockDeviceMappingType', {anyAttribs, 'item'}).
-record('ec2:UserDataType-data', {anyAttribs, 'data'}).
-record('ec2:UserDataType', {anyAttribs, 'version', 'encoding', choice}).
-record('ec2:GroupItemType', {anyAttribs, 'groupId'}).
-record('ec2:GroupSetType', {anyAttribs, 'item'}).
-record('ec2:RunInstancesType', {anyAttribs, 'imageId', 'minCount', 'maxCount', 'keyName', 'groupSet', 'additionalInfo', 'userData', 'addressingType', 'instanceType', 'placement', 'kernelId', 'ramdiskId', 'blockDeviceMapping', 'monitoring', 'subnetId', 'disableApiTermination', 'instanceInitiatedShutdownBehavior', 'license', 'privateIpAddress', 'clientToken'}).
-record('ec2:DescribeKeyPairsResponseItemType', {anyAttribs, 'keyName', 'keyFingerprint'}).
-record('ec2:DescribeKeyPairsResponseInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeKeyPairsResponseType', {anyAttribs, 'requestId', 'keySet'}).
-record('ec2:DescribeKeyPairsItemType', {anyAttribs, 'keyName'}).
-record('ec2:DescribeKeyPairsInfoType', {anyAttribs, 'item'}).
-record('ec2:DescribeKeyPairsType', {anyAttribs, 'keySet', 'filterSet'}).
-record('ec2:DeleteKeyPairResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeleteKeyPairType', {anyAttribs, 'keyName'}).
-record('ec2:ImportKeyPairResponseType', {anyAttribs, 'requestId', 'keyName', 'keyFingerprint'}).
-record('ec2:ImportKeyPairType', {anyAttribs, 'keyName', 'publicKeyMaterial'}).
-record('ec2:CreateKeyPairResponseType', {anyAttribs, 'requestId', 'keyName', 'keyFingerprint', 'keyMaterial'}).
-record('ec2:CreateKeyPairType', {anyAttribs, 'keyName'}).
-record('ec2:DeregisterImageResponseType', {anyAttribs, 'requestId', 'return'}).
-record('ec2:DeregisterImageType', {anyAttribs, 'imageId'}).
-record('ec2:RegisterImageResponseType', {anyAttribs, 'requestId', 'imageId'}).
-record('ec2:RegisterImageType', {anyAttribs, 'imageLocation', 'name', 'description', 'architecture', 'kernelId', 'ramdiskId', 'rootDeviceName', 'blockDeviceMapping'}).
-record('ec2:ProductCodeSetType', {anyAttribs, 'item'}).
-record('ec2:ProductCodeType', {anyAttribs, 'productCode'}).
-record('ec2:CreateImageResponseType', {anyAttribs, 'requestId', 'imageId'}).
-record('ec2:CreateImageType', {anyAttribs, 'instanceId', 'name', 'description', 'noReboot'}).