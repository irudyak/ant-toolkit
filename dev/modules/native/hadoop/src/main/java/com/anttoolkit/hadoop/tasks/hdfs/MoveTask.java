package com.anttoolkit.hadoop.tasks.hdfs;

public class MoveTask extends CopyTask
{
	@Override
	protected boolean deleteSource()
	{
		return true;
	}
}
