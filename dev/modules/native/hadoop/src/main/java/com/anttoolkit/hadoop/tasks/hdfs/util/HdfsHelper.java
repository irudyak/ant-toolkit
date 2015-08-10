package com.anttoolkit.hadoop.tasks.hdfs.util;

import org.apache.hadoop.util.StringUtils;

public class HdfsHelper
{
	public static String formatSize(long size, boolean humanReadable)
	{
    	return humanReadable ?
				StringUtils.TraditionalBinaryPrefix.long2String(size, "", 1) :
				String.valueOf(size);
 	}
}
