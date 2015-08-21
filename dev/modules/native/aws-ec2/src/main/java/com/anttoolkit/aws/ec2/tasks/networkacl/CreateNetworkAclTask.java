package com.anttoolkit.aws.ec2.tasks.networkacl;

import java.util.*;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;
import com.anttoolkit.aws.ec2.tasks.networkacl.util.*;

public class CreateNetworkAclTask extends GenericEc2Task
{
	@Required("VPC id should be specified")
	private String vpcId;
	private String name;
	private String aclIdProperty;

	private List<InboundRules> inboundRules = new LinkedList<InboundRules>();
	private List<OutboundRules> outboundRules = new LinkedList<OutboundRules>();

	public void setVpcId(String id)
	{
		vpcId = id;
	}

	public void setName(String name)
	{
		this.name = name;
	}

	public void setAclIdProperty(String property)
	{
		this.aclIdProperty = property;
	}

	public void addConfiguredInboundRules(InboundRules rules)
	{
		inboundRules.add(rules);
	}

	public void addConfiguredOutboundRules(OutboundRules rules)
	{
		outboundRules.add(rules);
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateNetworkAclRequest request = new CreateNetworkAclRequest();
		request.setVpcId(vpcId);

		CreateNetworkAclResult result = getEc2Client().createNetworkAcl(request);

		if (name != null)
		{
			setResourceName(result.getNetworkAcl().getNetworkAclId(), name);
		}

		if (aclIdProperty != null)
		{
			this.setPropertyThreadSafe(aclIdProperty, result.getNetworkAcl().getNetworkAclId());
		}

		for (InboundRules rules : inboundRules)
		{
			rules.createRules(getEc2Client(), result.getNetworkAcl().getNetworkAclId());
		}

		for (OutboundRules rules : outboundRules)
		{
			rules.createRules(getEc2Client(), result.getNetworkAcl().getNetworkAclId());
		}
	}
}
