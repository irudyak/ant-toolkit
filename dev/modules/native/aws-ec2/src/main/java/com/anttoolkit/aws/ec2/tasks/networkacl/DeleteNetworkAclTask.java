package com.anttoolkit.aws.ec2.tasks.networkacl;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.common.annotations.*;
import com.anttoolkit.aws.ec2.tasks.*;

public class DeleteNetworkAclTask extends GenericEc2Task
{
	@Required("Network ACL id should be specified")
	private String aclId;

	public void setAclId(String id)
	{
		this.aclId = id;
	}

	@Override
	public void doWork() throws BuildException
	{
		DeleteNetworkAclRequest request = (new DeleteNetworkAclRequest()).withNetworkAclId(aclId);
		getEc2Client().deleteNetworkAcl(request);
	}
}
