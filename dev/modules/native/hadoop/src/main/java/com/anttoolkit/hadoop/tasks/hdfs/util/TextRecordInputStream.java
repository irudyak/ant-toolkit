package com.anttoolkit.hadoop.tasks.hdfs.util;

import java.io.*;

import org.apache.commons.io.*;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.apache.hadoop.io.*;
import org.apache.hadoop.util.*;

public class TextRecordInputStream extends InputStream
{
	SequenceFile.Reader reader;
	Writable key;
	Writable val;

	DataInputBuffer inbuf;
	DataOutputBuffer outbuf;

	public TextRecordInputStream(FileStatus fileStatus, Configuration conf) throws IOException
	{
		final Path fpath = fileStatus.getPath();
		reader = new SequenceFile.Reader(conf, SequenceFile.Reader.file(fpath));
		key = ReflectionUtils.newInstance(reader.getKeyClass().asSubclass(Writable.class), conf);
		val = ReflectionUtils.newInstance(reader.getValueClass().asSubclass(Writable.class), conf);
		inbuf = new DataInputBuffer();
		outbuf = new DataOutputBuffer();
	}

	@Override
	public int read() throws IOException {
		int ret;

		if (null == inbuf || -1 == (ret = inbuf.read()))
		{
			if (!reader.next(key, val))
			{
				return -1;
			}

			byte[] tmp = key.toString().getBytes(Charsets.UTF_8);
			outbuf.write(tmp, 0, tmp.length);
			outbuf.write('\t');
			tmp = val.toString().getBytes(Charsets.UTF_8);
			outbuf.write(tmp, 0, tmp.length);
			outbuf.write('\n');
			inbuf.reset(outbuf.getData(), outbuf.getLength());
			outbuf.reset();
			ret = inbuf.read();
		}

		return ret;
	}

	@Override
	public void close() throws IOException
	{
		reader.close();
		super.close();
	}
}
