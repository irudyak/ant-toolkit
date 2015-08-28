package com.anttoolkit.aws.ec2.tasks.subnet;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteSubnetTask extends GenericEc2Task
{
	@Required("Subnet id should be specified")
	private String subnetId;

	@Override
	public void doWork() throws BuildException
	{
		getEc2Client().deleteSubnet(new DeleteSubnetRequest(subnetId));
	}
}
