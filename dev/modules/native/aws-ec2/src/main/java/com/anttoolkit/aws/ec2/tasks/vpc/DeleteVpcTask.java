package com.anttoolkit.aws.ec2.tasks.vpc;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteVpcTask extends GenericEc2Task
{
	private DeleteVpcRequest request = new DeleteVpcRequest();

	public void setVpcId(String id)
	{
		request.setVpcId(id);
	}

	@Override
	public void doWork() throws BuildException
	{
		getEc2Client().deleteVpc(request);
	}

	@Override
	protected void validate()
	{
		if (request.getVpcId() == null || request.getVpcId().trim().isEmpty())
		{
			throw new BuildException("VPC id should be specified");
		}
	}
}
