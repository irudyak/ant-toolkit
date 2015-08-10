package com.anttoolkit.maestro.tasks.checkpoint;

import java.util.*;

import com.anttoolkit.maestro.tasks.utils.*;
import com.anttoolkit.maestro.common.*;

@Action("describe-checkpoints")
public class CheckpointLoopTask extends XmlItemLoopTask
{
	@Param(sorted = true)
	private List<String> instances = new LinkedList<String>();

	public void setVolumes(String instances)
	{
		this.populateParamValues(instances, this.instances);
	}
}
