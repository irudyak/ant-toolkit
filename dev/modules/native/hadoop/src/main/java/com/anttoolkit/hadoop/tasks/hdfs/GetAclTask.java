package com.anttoolkit.hadoop.tasks.hdfs;

import java.io.*;
import java.util.*;

import com.google.common.base.*;

import org.apache.hadoop.fs.*;
import org.apache.hadoop.fs.permission.*;
import org.apache.tools.ant.*;

import com.anttoolkit.hadoop.tasks.hadoop.*;
import com.anttoolkit.general.tasks.collections.array.util.*;

public class GetAclTask extends GenericHadoopTask
{
	private String path;
	private String array;

	public void setPath(String path)
	{
		this.path = path;
	}

	public void setArray(String array)
	{
		this.array = array;
	}

	@Override
	protected void doHadoopWork() throws BuildException
	{
		FileStatus status = getFileStatus(path);
		FsPermission perm = status.getPermission();

		AclStatus aclStatus;

		try
		{
			aclStatus = getRemoteFileSystem().getAclStatus(new Path(path));
		}
		catch (IOException e)
		{
			throw new BuildException("Failed to get ACL status for path: " + path, e);
		}

		List<AclEntry> entries = perm.getAclBit() ? aclStatus.getEntries() : Collections.<AclEntry> emptyList();
		ScopedAclEntries scopedEntries = new ScopedAclEntries(AclUtil.getAclFromPermAndEntries(perm, entries));

		populateAclEntriesForSingleScope(aclStatus, perm, scopedEntries.getAccessEntries());
		populateAclEntriesForSingleScope(aclStatus, perm, scopedEntries.getDefaultEntries());
	}

	@Override
	protected void hadoopValidate()
	{
		if (path == null || path.trim().isEmpty())
		{
			throw new BuildException("Path should be specified");
		}

		if (getFileStatus(path) == null)
		{
			throw new BuildException("Specified path doesn't exist: " + path);
		}

		if (array == null || array.trim().isEmpty())
		{
			throw new BuildException("Array should be specified");
		}

		if (ArrayManager.exists(array))
		{
			throw new BuildException("Specified array doesn't exist: " + array);
		}
	}

	private void populateAclEntriesForSingleScope(AclStatus aclStatus, FsPermission fsPerm, List<AclEntry> entries)
	{
		if (entries.isEmpty())
		{
			return;
		}

		if (AclUtil.isMinimalAcl(entries))
		{
			for (AclEntry entry: entries)
			{
				ArrayManager.add(array, entry.toString());
			}
		}
		else
		{
			for (AclEntry entry: entries)
			{
				populateExtendedAclEntry(aclStatus, fsPerm, entry);
			}
		}
	}

	private void populateExtendedAclEntry(AclStatus aclStatus, FsPermission fsPerm, AclEntry entry)
	{
		if (entry.getName() != null || entry.getType() == AclEntryType.GROUP)
		{
			FsAction entryPerm = entry.getPermission();
			FsAction effectivePerm = getEffectivePermission(aclStatus, entry, fsPerm);

			if (entryPerm != effectivePerm)
			{
				ArrayManager.add(array, String.format("%s\t#effective:%s", entry, effectivePerm.SYMBOL));
			}
			else
			{
				ArrayManager.add(array, entry.toString());
			}
		}
		else
		{
			ArrayManager.add(array, entry.toString());
		}
	}

	public FsAction getEffectivePermission(AclStatus aclStatus, AclEntry entry, FsPermission permArg)
			throws IllegalArgumentException
	{
		if ((entry.getName() != null || entry.getType() == AclEntryType.GROUP))
		{
			if (entry.getScope() == AclEntryScope.ACCESS)
			{
				FsAction entryPerm = entry.getPermission();
				return entryPerm.and(permArg.getGroupAction());
			}

			Preconditions.checkArgument(aclStatus.getEntries().contains(entry) && aclStatus.getEntries().size() >= 3,
					"Passed default ACL entry not found in the list of ACLs");
			// default mask entry for effective permission calculation will be the
			// penultimate entry. This can be mask entry in case of extended ACLs.
			// In case of minimal ACL, this is the owner group entry, and we end up
			// intersecting group FsAction with itself, which is a no-op.
			FsAction defaultMask = aclStatus.getEntries().get(aclStatus.getEntries().size() - 2).getPermission();
			FsAction entryPerm = entry.getPermission();
			return entryPerm.and(defaultMask);
		}
		else
		{
			return entry.getPermission();
		}
	}

}
