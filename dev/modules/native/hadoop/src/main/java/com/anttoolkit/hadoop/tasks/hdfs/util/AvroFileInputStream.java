package com.anttoolkit.hadoop.tasks.hdfs.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.apache.avro.*;
import org.apache.avro.file.*;
import org.apache.avro.generic.*;
import org.apache.avro.io.*;
import org.apache.commons.io.*;
import org.apache.hadoop.conf.*;
import org.apache.hadoop.fs.*;
import org.codehaus.jackson.*;
import org.codehaus.jackson.util.*;

public class AvroFileInputStream extends InputStream
{
	private int pos;
	private byte[] buffer;
	private ByteArrayOutputStream output;
	private FileReader<?> fileReader;
	private DatumWriter<Object> writer;
	private JsonEncoder encoder;

	public AvroFileInputStream(FileStatus status) throws IOException
	{
		pos = 0;
		buffer = new byte[0];
		GenericDatumReader<Object> reader = new GenericDatumReader<Object>();
		FileContext fc = FileContext.getFileContext(new Configuration());
		fileReader =DataFileReader.openReader(new AvroFSInput(fc, status.getPath()), reader);

		Schema schema = fileReader.getSchema();
		writer = new GenericDatumWriter<Object>(schema);
		output = new ByteArrayOutputStream();
		JsonGenerator generator = new JsonFactory().createJsonGenerator(output, JsonEncoding.UTF8);

		MinimalPrettyPrinter prettyPrinter = new MinimalPrettyPrinter();
		prettyPrinter.setRootValueSeparator(System.getProperty("line.separator"));
		generator.setPrettyPrinter(prettyPrinter);

		encoder = EncoderFactory.get().jsonEncoder(schema, generator);
	}

	/**
	 * Read a single byte from the stream.
	 */
	@Override
	public int read() throws IOException
	{
		if (pos < buffer.length)
		{
			return buffer[pos++];
		}

		if (!fileReader.hasNext())
		{
			return -1;
		}

		writer.write(fileReader.next(), encoder);
		encoder.flush();

		if (!fileReader.hasNext())
		{
			// Write a new line after the last Avro record.
			output.write(System.getProperty("line.separator").getBytes(Charsets.UTF_8));
			output.flush();
		}

		pos = 0;
		buffer = output.toByteArray();
		output.reset();

		return read();
	}

	/**
	 * Close the stream.
	 */
	@Override
	public void close() throws IOException
	{
		fileReader.close();
		output.close();
		super.close();
	}
}