package com.anttoolkit.aws.ec2.tasks.instance;

import java.text.*;

import com.amazonaws.services.ec2.model.*;

import com.anttoolkit.general.common.*;

public abstract class InstanceInfoTask extends InstanceTask
{
	private static final SimpleDateFormat TIME_FORMATTER = new SimpleDateFormat("MM/dd/yyyy hh:mm:ss");

	private SimpleDateFormat formatter = TIME_FORMATTER;

	private String ownerIdProperty;
	private String requesterIdProperty;
	private String reservationIdProperty;

	private String instanceIdProperty;
	private String amiLaunchIndexProperty;
	private String architectureProperty;
	private String blockDeviceMappingsProperty;
	private String clientTokenProperty;
	private String ebsOptimizedProperty;
	private String hypervisorProperty;
	private String iamInstanceProfileProperty;
	private String imageIdProperty;
	private String lifecycleProperty;
	private String typeProperty;
	private String kernelIdProperty;
	private String keyNameProperty;
	private String launchTimeProperty;
	private String monitoringProperty;
	private String networkInterfacesProperty;
	private String placementProperty;
	private String platformProperty;
	private String privateDnsProperty;
	private String privateIpProperty;
	private String productCodesProperty;
	private String publicDnsProperty;
	private String publicIpProperty;
	private String ramdiskIdProperty;
	private String rootDeviceNameProperty;
	private String rootDeviceTypeProperty;
	private String securityGroupsProperty;
	private String sourceDestCheckProperty;
	private String sriovNetSupportProperty;
	private String stateProperty;
	private String subnetIdProperty;
	private String tagsProperty;

	public void setTimeFormat(String format)
	{
		this.formatter =  new SimpleDateFormat(format);
	}

	public void setOwnerIdProperty(String property)
	{
		this.ownerIdProperty = property;
	}

	public void setRequesterIdProperty(String property)
	{
		this.requesterIdProperty = property;
	}

	public void setReservationIdProperty(String property)
	{
		this.reservationIdProperty = property;
	}

	public void setInstanceIdProperty(String property)
	{
		this.instanceIdProperty = property;
	}

	public void setAmiLaunchIndexProperty(String property)
	{
		this.amiLaunchIndexProperty = property;
	}

	public void setArchitectureProperty(String property)
	{
		this.architectureProperty = property;
	}

	public void setBlockDeviceMappingsProperty(String property)
	{
		this.blockDeviceMappingsProperty = property;
	}

	public void setClientTokenProperty(String property)
	{
		this.clientTokenProperty = property;
	}

	public void setEbsOptimizedProperty(String property)
	{
		this.ebsOptimizedProperty = property;
	}

	public void setHypervisorProperty(String property)
	{
		this.hypervisorProperty = property;
	}

	public void setIamInstanceProfileProperty(String property)
	{
		this.iamInstanceProfileProperty = property;
	}

	public void setImageIdProperty(String property)
	{
		this.imageIdProperty = property;
	}

	public void setLifecycleProperty(String property)
	{
		this.lifecycleProperty = property;
	}

	public void setTypeProperty(String property)
	{
		this.typeProperty = property;
	}

	public void setKernelIdProperty(String property)
	{
		this.kernelIdProperty = property;
	}

	public void setKeyNameProperty(String property)
	{
		this.keyNameProperty = property;
	}

	public void setLaunchTimeProperty(String property)
	{
		this.launchTimeProperty = property;
	}

	public void setMonitoringProperty(String property)
	{
		this.monitoringProperty = property;
	}

	public void setNetworkInterfacesProperty(String property)
	{
		this.networkInterfacesProperty = property;
	}

	public void setPlacementProperty(String property)
	{
		this.placementProperty = property;
	}

	public void setPlatformProperty(String property)
	{
		this.platformProperty = property;
	}

	public void setPrivateDnsProperty(String property)
	{
		this.privateDnsProperty = property;
	}

	public void setPrivateIpProperty(String property)
	{
		this.privateIpProperty = property;
	}

	public void setProductCodesProperty(String property)
	{
		this.productCodesProperty = property;
	}

	public void setPublicDnsProperty(String property)
	{
		this.publicDnsProperty = property;
	}

	public void setPublicIpProperty(String property)
	{
		this.publicIpProperty = property;
	}

	public void setRamdiskIdProperty(String property)
	{
		this.ramdiskIdProperty = property;
	}

	public void setRootDeviceNameProperty(String property)
	{
		this.rootDeviceNameProperty = property;
	}

	public void setRootDeviceTypeProperty(String property)
	{
		this.rootDeviceTypeProperty = property;
	}

	public void setSecurityGroupsProperty(String property)
	{
		this.securityGroupsProperty = property;
	}

	public void setSourceDestCheckProperty(String property)
	{
		this.sourceDestCheckProperty = property;
	}

	public void setSriovNetSupportProperty(String property)
	{
		this.sriovNetSupportProperty = property;
	}

	public void setStateProperty(String property)
	{
		this.stateProperty = property;
	}

	public void setSubnetIdProperty(String property)
	{
		this.subnetIdProperty = property;
	}

	public void setTagsProperty(String property)
	{
		this.tagsProperty = property;
	}

	protected void setPropertiesFromInstance(Instance instance)
	{
		if (instanceIdProperty != null)
		{
			this.setPropertyThreadSafe(instanceIdProperty, instance.getInstanceId());
		}

		if (amiLaunchIndexProperty != null)
		{
			this.setPropertyThreadSafe(amiLaunchIndexProperty, Integer.toString(instance.getAmiLaunchIndex()));
		}

		if (architectureProperty != null)
		{
			this.setPropertyThreadSafe(architectureProperty, instance.getArchitecture());
		}

		if (blockDeviceMappingsProperty != null)
		{
			this.setPropertyThreadSafe(blockDeviceMappingsProperty, CollectionsHelper.toString(instance.getBlockDeviceMappings()));
		}

		if (clientTokenProperty != null)
		{
			this.setPropertyThreadSafe(clientTokenProperty, instance.getClientToken());
		}

		if (ebsOptimizedProperty != null)
		{
			this.setPropertyThreadSafe(ebsOptimizedProperty, Boolean.toString(instance.getEbsOptimized()));
		}

		if (hypervisorProperty != null)
		{
			this.setPropertyThreadSafe(hypervisorProperty, instance.getHypervisor());
		}

		if (iamInstanceProfileProperty != null)
		{
			this.setPropertyThreadSafe(iamInstanceProfileProperty, instance.getIamInstanceProfile() != null ? instance.getIamInstanceProfile().toString() : "");
		}

		if (imageIdProperty != null)
		{
			this.setPropertyThreadSafe(imageIdProperty, instance.getImageId());
		}

		if (lifecycleProperty != null)
		{
			this.setPropertyThreadSafe(lifecycleProperty, instance.getInstanceLifecycle());
		}

		if (typeProperty != null)
		{
			this.setPropertyThreadSafe(typeProperty, instance.getInstanceType());
		}

		if (kernelIdProperty != null)
		{
			this.setPropertyThreadSafe(kernelIdProperty, instance.getKernelId());
		}

		if (keyNameProperty != null)
		{
			this.setPropertyThreadSafe(keyNameProperty, instance.getKeyName());
		}

		if (launchTimeProperty != null)
		{
			this.setPropertyThreadSafe(launchTimeProperty, formatter.format(instance.getLaunchTime()));
		}

		if (monitoringProperty != null)
		{
			this.setPropertyThreadSafe(monitoringProperty, instance.getMonitoring() != null ? instance.getMonitoring().toString() : "");
		}

		if (networkInterfacesProperty != null)
		{
			this.setPropertyThreadSafe(networkInterfacesProperty, CollectionsHelper.toString(instance.getNetworkInterfaces()));
		}

		if (placementProperty != null)
		{
			this.setPropertyThreadSafe(placementProperty, instance.getPlacement() != null ? instance.getPlacement().toString() : null);
		}

		if (platformProperty != null)
		{
			this.setPropertyThreadSafe(platformProperty, instance.getPlatform());
		}

		if (privateDnsProperty != null)
		{
			this.setPropertyThreadSafe(privateDnsProperty, instance.getPrivateDnsName());
		}

		if (privateIpProperty != null)
		{
			this.setPropertyThreadSafe(privateIpProperty, instance.getPrivateIpAddress());
		}

		if (productCodesProperty != null)
		{
			this.setPropertyThreadSafe(productCodesProperty, CollectionsHelper.toString(instance.getProductCodes()));
		}

		if (publicDnsProperty != null)
		{
			this.setPropertyThreadSafe(publicDnsProperty, instance.getPublicDnsName());
		}

		if (publicIpProperty != null)
		{
			this.setPropertyThreadSafe(publicIpProperty, instance.getPublicIpAddress());
		}

		if (ramdiskIdProperty != null)
		{
			this.setPropertyThreadSafe(ramdiskIdProperty, instance.getRamdiskId());
		}

		if (rootDeviceNameProperty != null)
		{
			this.setPropertyThreadSafe(rootDeviceNameProperty, instance.getRootDeviceName());
		}

		if (rootDeviceTypeProperty != null)
		{
			this.setPropertyThreadSafe(rootDeviceTypeProperty, instance.getRootDeviceType());
		}

		if (securityGroupsProperty != null)
		{
			this.setPropertyThreadSafe(securityGroupsProperty, CollectionsHelper.toString(instance.getSecurityGroups()));
		}

		if (sourceDestCheckProperty != null)
		{
			this.setPropertyThreadSafe(sourceDestCheckProperty, Boolean.toString(instance.getSourceDestCheck()));
		}

		if (sriovNetSupportProperty != null)
		{
			this.setPropertyThreadSafe(sriovNetSupportProperty, instance.getSriovNetSupport());
		}

		if (stateProperty != null)
		{
			this.setPropertyThreadSafe(stateProperty, instance.getState().getName());
		}

		if (subnetIdProperty != null)
		{
			this.setPropertyThreadSafe(subnetIdProperty, instance.getSubnetId());
		}

		if (tagsProperty != null)
		{
			this.setPropertyThreadSafe(tagsProperty, CollectionsHelper.toString(instance.getTags()));
		}
	}

	protected void setPropertiesFromReservation(Reservation res)
	{
		if (ownerIdProperty != null)
		{
			this.setPropertyThreadSafe(ownerIdProperty, res.getOwnerId());
		}

		if (requesterIdProperty != null)
		{
			this.setPropertyThreadSafe(requesterIdProperty, res.getRequesterId());
		}

		if (reservationIdProperty != null)
		{
			this.setPropertyThreadSafe(reservationIdProperty, res.getReservationId());
		}
	}
}
