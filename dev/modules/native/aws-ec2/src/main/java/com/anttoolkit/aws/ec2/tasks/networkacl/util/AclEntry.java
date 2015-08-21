package com.anttoolkit.aws.ec2.tasks.networkacl.util;

import com.amazonaws.services.ec2.model.*;

import org.apache.tools.ant.*;

public class AclEntry
{
	private CreateNetworkAclEntryRequest request = new CreateNetworkAclEntryRequest();

	public void setProtocol(String protocol)
	{
		request.setProtocol(protocol);
	}

	public void setAction(String action)
	{
		if (action == null)
		{
			throw new BuildException("Can't specify empty action for network ACL entry");
		}

		request.setRuleAction(action.toLowerCase().trim());
	}

	public void setCidrBlock(String cidrBlock)
	{
		request.setCidrBlock(cidrBlock);
	}

	public void setIcmpTypeCode(String val)
	{
		if (val == null)
		{
			return;
		}

		String[] chunks = val.split(",");

		try
		{
			Integer type = Integer.parseInt(chunks[0].trim());
			Integer code = chunks.length > 1 ? Integer.parseInt(chunks[1].trim()) : null;

			IcmpTypeCode typeCode = new IcmpTypeCode();

			typeCode.setType(type);

			if (code != null)
			{
				typeCode.setCode(code);
			}

			request.setIcmpTypeCode(typeCode);
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect ICMP type/code specified: " + val);
		}
	}

	public void setPortRange(String val)
	{
		if (val == null)
		{
			return;
		}

		String[] range = val.split("-");
		if (range.length != 2)
		{
			throw new BuildException("Incorrect port range specified");
		}

		try
		{
			PortRange portRange = new PortRange();
			portRange.setFrom(Integer.parseInt(range[0].trim()));
			portRange.setTo(Integer.parseInt(range[1].trim()));
			request.setPortRange(portRange);
		}
		catch (NumberFormatException e)
		{
			throw new BuildException("Incorrect port range specified: " + val);
		}
	}

	public CreateNetworkAclEntryRequest getRequest(int ruleNumber, boolean egress)
	{
		request.setRuleNumber(ruleNumber);
		request.setEgress(egress);
		return request;
	}
}
