package com.anttoolkit.hbase.tasks.data.util;

import org.apache.hadoop.hbase.client.*;

import com.anttoolkit.hbase.tasks.*;

public interface IAction<A extends Row>
{
	public A getAction();
	public void execute(HTableInterface table);
	public void processExecutionResult(Result result, String table);
	public void setRowkey(String rowkey);
	public void setRowkeyRaw(byte[] rowkey);
	public void setRowkeyRef(String reference);
	public void setRowkeyType(String type);
	public CellValueType getRowkeyType();
	public byte[] getRowkey();
	public String getRowkeyAsString();
	public void validate();
}
