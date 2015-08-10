package com.anttoolkit.aws.types;

import java.net.*;
import java.util.*;

import com.amazonaws.*;
import com.amazonaws.auth.profile.*;
import com.amazonaws.internal.*;
import com.amazonaws.auth.*;
import com.amazonaws.regions.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;
import com.anttoolkit.general.common.*;

public class AwsConfig
{
	private static final Region DEFAULT_REGION = Region.getRegion(Regions.US_WEST_2);

	private static final Set<String> CLIENT_CONFIG_SUPPORTED_PROPS = new HashSet<String>()
	{{
		add("connectiontimeout");
		add("maxconnections");
		add("maxerrorretry");
		add("localaddress");
		add("protocol");
		add("proxydomain");
		add("proxyhost");
		add("proxypassword");
		add("proxyport");
		add("proxyusername");
		add("proxyworkstation");
		add("preemptivebasicproxyauth");
		add("sockettimeout");
		add("useragent");
		add("usereaper");
		add("usegzip");
		add("socketreceivebuffersizehint");
		add("socketsendbuffersizehint");
		add("signeroverride");
		add("connectionttl");
		add("tcpkeepalive");
		add("responsemetadatacachesize");
	}};

	private boolean useInstanceProfileCredentials;
	private String accessKeyId;
	private String secretAccessKey;
	private String sessionToken;
	private String credentialsFile;
	private String profilesConfigFile;
	private String profile;

	private String region;
	private String endpoint;
	private String availabilityZone;

	private Map<String, String> clientConfigProps = new HashMap<String, String>();

	public void setUseInstanceProfileCredentials(boolean useInstanceProfileCredentials)
	{
		this.useInstanceProfileCredentials = useInstanceProfileCredentials;
	}

	public void setCredentialsFile(String credentialsFile)
	{
		this.credentialsFile = credentialsFile;
	}

	public void setProfilesConfigFile(String file)
	{
		this.profilesConfigFile = file;
	}

	public void setProfile(String profile)
	{
		this.profile = profile;
	}

	public void setAccessKeyId(String accessKeyId)
	{
		this.accessKeyId = accessKeyId;
	}

	public void setSecretAccessKey(String secretAccessKey)
	{
		this.secretAccessKey = secretAccessKey;
	}

	public void setSessionToken(String sessionToken)
	{
		this.sessionToken = sessionToken;
	}

	public void setRegion(String region)
	{
		if (endpoint != null)
		{
			throw new BuildException("Region and endpoint are interchangeable things and could not be set both at the same time");
		}

		this.region = region;
	}

	public void setAvailabilityZone(String availabilityZone)
	{
		this.availabilityZone = availabilityZone;
	}

	public void setEndpoint(String endpoint)
	{
		if (region != null)
		{
			throw new BuildException("Region and endpoint are interchangeable things and could not be set both at the same time");
		}

		this.endpoint = endpoint;
	}

	public void addConfiguredProperty(NameValueHolder holder)
	{
		String property = holder.getName().toLowerCase().trim();
		String value = holder.getValue().trim();

		if (!CLIENT_CONFIG_SUPPORTED_PROPS.contains(property))
		{
			throw new BuildException("Client config property '" + holder.getName() + "' isn't supported");
		}

		if ("protocol".equals(property) &&
			!Protocol.HTTP.toString().equals(value.toLowerCase()) &&
			!Protocol.HTTPS.toString().equals(value.toLowerCase()))
		{
			throw new BuildException("Incorrect value '" + value +
					"' specified for '" + holder.getName().trim() + "' client config property");
		}

		try
		{
			if ("localaddress".equals(property))
			{
				InetAddress.getByName(value);
			}

			if ("maxConnections".equals(property) || "socketTimeout".equals(property) ||
				"connectionTimeout".equals(property) || "socketSendBufferSizeHint".equals(property) ||
				"socketReceiveBufferSizeHint".equals(property) || "responseMetadataCacheSize".equals(property) ||
				"proxyPort".equals(property) || "maxErrorRetry".equals(property))
			{
				Integer.parseInt(value);
			}

			if ("connectionttl".equals(property))
			{
				Long.parseLong(value);
			}

			if ("preemptivebasicproxyauth".equals(property) || "usereaper".equals(property) ||
				"usegzip".equals(property) || "tcpkeepalive".equals(property))
			{
				BooleanHelper.getBoolean(value);
			}
		}
		catch (Throwable e)
		{
			throw new BuildException("Incorrect value '" + value +
					"' specified for '" + holder.getName().trim() + "' client config property", e);
		}

		clientConfigProps.put(property, value);
	}

	public Region getRegion()
	{
		if (region != null)
		{
			Region reg = RegionUtils.getRegion(region);
			if (reg == null)
			{
				throw new BuildException("Failed to identify AWS region with name '" + region + "'");
			}

			return reg;
		}

		if (endpoint != null)
		{
			Region reg = RegionUtils.getRegionByEndpoint(endpoint);

			if (reg == null)
			{
				throw new BuildException("Failed to identify AWS region by endpoint '" + endpoint + "'");
			}

			return reg;
		}

		return DEFAULT_REGION;
	}

	public String getEndpoint()
	{
		return endpoint;
	}

	public String getAvailabilityZone()
	{
		return availabilityZone;
	}

	public ClientConfiguration getClientConfiguration()
	{
		if (clientConfigProps.isEmpty())
		{
			return null;
		}

		ClientConfiguration conf = new ClientConfiguration();

		if (clientConfigProps.containsKey("connectiontimeout"))
		{
			conf.setConnectionTimeout(Integer.parseInt(clientConfigProps.get("connectiontimeout")));
		}

		if (clientConfigProps.containsKey("maxconnections"))
		{
			conf.setMaxConnections(Integer.parseInt(clientConfigProps.get("maxconnections")));
		}

		if (clientConfigProps.containsKey("maxerrorretry"))
		{
			conf.setMaxErrorRetry(Integer.parseInt(clientConfigProps.get("maxerrorretry")));
		}

		if (clientConfigProps.containsKey("localaddress"))
		{
			try
			{
				conf.setLocalAddress(InetAddress.getByName(clientConfigProps.get("localaddress")));
			}
			catch (UnknownHostException e)
			{
				throw new BuildException("Failed to set local address client property to: " + clientConfigProps.get("localaddress"), e);
			}
		}

		if (clientConfigProps.containsKey("protocol"))
		{
			String val = clientConfigProps.get("protocol").toLowerCase();
			conf.setProtocol(Protocol.HTTP.toString().equals(val) ? Protocol.HTTP : Protocol.HTTPS);
		}

		if (clientConfigProps.containsKey("proxydomain"))
		{
			conf.setProxyDomain(clientConfigProps.get("proxydomain"));
		}

		if (clientConfigProps.containsKey("proxyhost"))
		{
			conf.setProxyHost(clientConfigProps.get("proxyhost"));
		}

		if (clientConfigProps.containsKey("proxypassword"))
		{
			conf.setProxyPassword(clientConfigProps.get("proxypassword"));
		}

		if (clientConfigProps.containsKey("proxyport"))
		{
			conf.setProxyPort(Integer.parseInt(clientConfigProps.get("proxyport")));
		}

		if (clientConfigProps.containsKey("proxyusername"))
		{
			conf.setProxyUsername(clientConfigProps.get("proxyusername"));
		}

		if (clientConfigProps.containsKey("proxyworkstation"))
		{
			conf.setProxyWorkstation(clientConfigProps.get("proxyworkstation"));
		}

		if (clientConfigProps.containsKey("preemptivebasicproxyauth"))
		{
			conf.setPreemptiveBasicProxyAuth(BooleanHelper.getBoolean(clientConfigProps.get("preemptivebasicproxyauth")));
		}

		if (clientConfigProps.containsKey("sockettimeout"))
		{
			conf.setSocketTimeout(Integer.parseInt(clientConfigProps.get("sockettimeout")));
		}

		if (clientConfigProps.containsKey("useragent"))
		{
			conf.setUserAgent(clientConfigProps.get("useragent"));
		}

		if (clientConfigProps.containsKey("usereaper"))
		{
			conf.setUseReaper(BooleanHelper.getBoolean(clientConfigProps.get("usereaper")));
		}

		if (clientConfigProps.containsKey("usegzip"))
		{
			conf.setUseGzip(BooleanHelper.getBoolean(clientConfigProps.get("usegzip")));
		}

		if (clientConfigProps.containsKey("socketreceivebuffersizehint") || clientConfigProps.containsKey("socketsendbuffersizehint"))
		{
			int sendSize = clientConfigProps.containsKey("socketsendbuffersizehint") ?
					Integer.parseInt(clientConfigProps.get("socketsendbuffersizehint")): 0;

			int recvSize = clientConfigProps.containsKey("socketreceivebuffersizehint") ?
					Integer.parseInt(clientConfigProps.get("socketreceivebuffersizehint")): 0;

			conf.setSocketBufferSizeHints(sendSize, recvSize);
		}

		if (clientConfigProps.containsKey("signeroverride"))
		{
			conf.setSignerOverride(clientConfigProps.get("signeroverride"));
		}

		if (clientConfigProps.containsKey("connectionttl"))
		{
			conf.setConnectionTTL(Long.parseLong(clientConfigProps.get("connectionttl")));
		}

		if (clientConfigProps.containsKey("tcpkeepalive"))
		{
			conf.setUseTcpKeepAlive(BooleanHelper.getBoolean(clientConfigProps.get("tcpkeepalive")));
		}

		if (clientConfigProps.containsKey("responsemetadatacachesize"))
		{
			conf.setResponseMetadataCacheSize(Integer.parseInt(clientConfigProps.get("responsemetadatacachesize")));
		}

		return conf;
	}

	public AWSCredentialsProvider getCredentialsProvider(GenericTask task)
	{
		AWSCredentialsProvider provider = getInstanceProfileCredentialsProvider();
		if (provider != null)
		{
			return provider;
		}

		provider = getExplicitlySpecifiedCredentialsProvider();
		if (provider != null)
		{
			return provider;
		}

		provider = getCredentialsFileCredentialsProvider(task);
		if (provider != null)
		{
			return provider;
		}

		provider = getProfileCredentialsProvider(task);
		if (provider != null)
		{
			return provider;
		}

		return new DefaultAWSCredentialsProviderChain();
	}

	private AWSCredentialsProvider getInstanceProfileCredentialsProvider()
	{
		if (!useInstanceProfileCredentials)
		{
			return null;
		}

		if (credentialsFile != null)
		{
			throw new BuildException("Credentials file '" + credentialsFile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (profilesConfigFile != null)
		{
			throw new BuildException("Profiles config file '" + profilesConfigFile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (profile != null)
		{
			throw new BuildException("Credentials profile '" + profile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (accessKeyId != null)
		{
			throw new BuildException("Access Key ID can't be used while using Instance Profile Credentials authentication");
		}

		if (secretAccessKey != null)
		{
			throw new BuildException("Secret Access Key can't be used while using Instance Profile Credentials authentication");
		}

		if (sessionToken != null)
		{
			throw new BuildException("STS Session Token can't be used while using Instance Profile Credentials authentication");
		}

		return new InstanceProfileCredentialsProvider();
	}

	private AWSCredentialsProvider getExplicitlySpecifiedCredentialsProvider()
	{
		if (accessKeyId == null && secretAccessKey == null && sessionToken == null)
		{
			return null;
		}

		if (credentialsFile != null)
		{
			throw new BuildException("Credentials file '" + credentialsFile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (profilesConfigFile != null)
		{
			throw new BuildException("Profiles config file '" + profilesConfigFile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (profile != null)
		{
			throw new BuildException("Credentials profile '" + profile + "' can't be used while using Instance Profile Credentials authentication");
		}

		if (sessionToken == null)
		{
			if (accessKeyId == null)
			{
				throw new BuildException("Access Key ID should be specified for explicit authentication");
			}

			if (secretAccessKey == null)
			{
				throw new BuildException("Secret Access Key should be specified for explicit authentication");
			}

			return new StaticCredentialsProvider(new BasicAWSCredentials(accessKeyId, secretAccessKey));
		}

		if (accessKeyId == null && secretAccessKey == null)
		{
			throw new BuildException("Access Key ID and Secret Access Key should be specified for STS token authentication");
		}

		if (accessKeyId == null)
		{
			throw new BuildException("Access Key ID should be specified for STS token authentication");
		}

		if (secretAccessKey == null)
		{
			throw new BuildException("Secret Access Key should be specified for STS token authentication");
		}

		return new StaticCredentialsProvider(new BasicSessionCredentials(accessKeyId, secretAccessKey, sessionToken));
	}

	private AWSCredentialsProvider getCredentialsFileCredentialsProvider(GenericTask task)
	{
		if (credentialsFile == null)
		{
			return null;
		}

		if (!task.fileExists(credentialsFile))
		{
			throw new BuildException("Specified credentials file doesn't exist: " + credentialsFile);
		}

		if (profilesConfigFile != null)
		{
			throw new BuildException("Profiles config file '" + profilesConfigFile + "' can't be used while using credentials file authentication");
		}

		if (profile != null)
		{
			throw new BuildException("Credentials profile '" + profile + "' can't be used while using credentials file authentication");
		}

		return new PropertiesFileCredentialsProvider(task.getFileFullPath(credentialsFile));
	}

	private AWSCredentialsProvider getProfileCredentialsProvider(GenericTask task)
	{
		if (profilesConfigFile == null && profile == null)
		{
			return null;
		}

		if (profilesConfigFile != null && !task.fileExists(profilesConfigFile))
		{
			throw new BuildException("Specified profiles config file doesn't exist: " + profilesConfigFile);
		}

		if (profile == null)
		{
			return new ProfileCredentialsProvider(task.getFileFullPath(profilesConfigFile), "default");
		}

		if (profilesConfigFile == null)
		{
			return new ProfileCredentialsProvider(profile);
		}

		return new ProfileCredentialsProvider(profile, task.getFileFullPath(profilesConfigFile));
	}
}
