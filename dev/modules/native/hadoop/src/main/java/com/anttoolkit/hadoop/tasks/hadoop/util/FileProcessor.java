package com.anttoolkit.hadoop.tasks.hadoop.util;

import org.apache.hadoop.fs.*;

public interface FileProcessor
{
	public void process(FileStatus status);
}
