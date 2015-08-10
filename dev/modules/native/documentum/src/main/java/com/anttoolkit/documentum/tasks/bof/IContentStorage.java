package com.anttoolkit.documentum.tasks.bof;

public interface IContentStorage
{
	public DocAppObject putContent(String objectId,
								   String name,
								   String objectType);
}
