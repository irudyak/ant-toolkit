package com.anttoolkit.aws.ec2.tasks.vpc;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class CreateVpcTask extends GenericEc2Task
{
	private String name;
	private String vpcIdProperty;
	private CreateVpcRequest request = new CreateVpcRequest();

	public void setName(String name)
	{
		this.name = name;
	}

	public void setCidrBlock(String cidrBlock)
	{
		request.setCidrBlock(cidrBlock);
	}

	public void setTenancy(String tenancy)
	{
		request.setInstanceTenancy(tenancy.toLowerCase().trim());
	}

	public void setVpcIdProperty(String vpcIdProperty)
	{
		this.vpcIdProperty = vpcIdProperty;
	}

	@Override
	public void doWork() throws BuildException
	{
		CreateVpcResult result = getEc2Client().createVpc(request);

		if (vpcIdProperty != null)
		{
			this.setPropertyThreadSafe(vpcIdProperty, result.getVpc().getVpcId());
		}

		if (name != null)
		{
			createTags(result.getVpc().getVpcId(), new Tag("name", name));
		}
	}
}
