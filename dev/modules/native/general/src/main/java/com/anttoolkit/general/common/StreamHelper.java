package com.anttoolkit.general.common;

import java.io.*;
import java.net.*;

public class StreamHelper
{
	private static void copyBytes(InputStream in, OutputStream out, int buffSize, boolean close)
			throws IOException
	{
		try
		{
			copyBytes(in, out, buffSize);

			if (close)
			{
				out.close();
				out = null;
				in.close();
				in = null;
			}
		}
		finally
		{
			if (close)
			{
				closeStream(out);
				closeStream(in);
			}
		}
	}

	public static void copyBytes(InputStream in, OutputStream out, int buffSize)
			throws IOException
	{
		PrintStream ps = out instanceof PrintStream ? (PrintStream) out : null;
		byte[] buf = new byte[buffSize];

		for (int bytesRead = in.read(buf); bytesRead >= 0; bytesRead = in.read(buf))
		{
			out.write(buf, 0, bytesRead);
			if (ps != null && ps.checkError())
			{
				throw new IOException("Unable to write to output stream.");
			}
		}
	}

	public static void cleanup(Closeable... closeables)
	{
		for (Closeable closeable : closeables)
		{
			if (closeable != null)
			{
				try
				{
					closeable.close();
				}
				catch (IOException e)
				{
				}
			}
		}
	}

	public static void closeStream(Closeable stream)
	{
		cleanup(stream);
	}

	public static void closeSocket(Socket sock)
	{
		if (sock != null)
		{
			try
			{
				sock.close();
			}
			catch (IOException e)
			{
			}
		}
	}
}
