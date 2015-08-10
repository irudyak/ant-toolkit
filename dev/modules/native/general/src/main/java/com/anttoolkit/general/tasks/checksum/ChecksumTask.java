package com.anttoolkit.general.tasks.checksum;

import java.io.*;
import java.security.*;
import javax.xml.bind.*;

import org.apache.tools.ant.*;

import com.anttoolkit.general.tasks.*;

public abstract class ChecksumTask extends GenericTask
{
	private static final String MD5 = "MD5";
	private static final String SHA_1 = "SHA-1";
	private static final String SHA_256 = "SHA-256";

	private String algorithm = MD5;

	public void setAlgorithm(String algorithm)
	{
		if (algorithm == null)
		{
			throw new BuildException("Can't specify null checksum algorithm");
		}

		if (!MD5.equalsIgnoreCase(algorithm) &&
			!SHA_1.equalsIgnoreCase(algorithm) &&
			!SHA_256.equalsIgnoreCase(algorithm))
		{
			throw new BuildException("Incorrect checksum algorithm specified");
		}

		this.algorithm = algorithm;
	}

	protected String getAlgorithm()
	{
		return algorithm;
	}

	protected MessageDigest getDigest()
	{
		try
		{
			return MessageDigest.getInstance(algorithm);
		}
		catch (NoSuchAlgorithmException e)
		{
			throw new BuildException("There are no specified algorithm '" + algorithm + "' for message digest", e);
		}
	}

	protected String generateChecksum(String str)
	{
		try
		{
			MessageDigest digest = getDigest();
			getDigest().update(str.getBytes("UTF-8"));
			return DatatypeConverter.printHexBinary(digest.digest());
		}
		catch (UnsupportedEncodingException e)
		{
			throw new BuildException(e);
		}
	}

	protected String generateChecksum(File file)
	{
		FileInputStream in = null;

		try
		{
			MessageDigest digest = getDigest();
			byte[] dataBytes = new byte[1024];
			int readBytes;

			in = new FileInputStream(file);

			while ((readBytes = in.read(dataBytes)) != -1)
			{
				digest.update(dataBytes, 0, readBytes);
			}

			return DatatypeConverter.printHexBinary(digest.digest());
		}
		catch (FileNotFoundException e)
		{
			throw new BuildException("Failed to find file '" + file + "' for which to generate " + getAlgorithm() + " checksum", e);
		}
		catch (IOException e)
		{
			throw new BuildException(e);
		}
		finally
		{
			if (in != null)
			{
				try
				{
					in.close();
				}
				catch (Throwable e){}
			}
		}
	}
}
