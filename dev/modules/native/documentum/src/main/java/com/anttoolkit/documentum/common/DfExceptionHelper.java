package com.anttoolkit.documentum.common;

import com.documentum.fc.common.*;

public class DfExceptionHelper
{

	/**
	 * Object version mistmatch error (optimistic locking feature)
	 */
	public static final String OBJECT_VERSION_MISTMATCH[] = {
			"DM_OBJ_MGR_E_VERSION_MISMATCH",
			"DM_SYSOBJECT_E_VERSION_MISMATCH"};

	/**
	 * Failed to fetch object error
	 */
	public static final String OBJECT_FETCH_FAILED_ERROR = "DM_OBJ_MGR_E_FETCH_FAIL";

	/**
	 * Failed to fetch object with invalid id
	 */
	public static final String INVALID_OBJECT_FETCH_FAILED_ERROR = "DM_SYSOBJECT_E_CANT_FETCH_INVALID_ID";

	/**
	 * Failed to open cursor in RDBMS error
	 */
	public static final String CURSOR_FAIL_ERROR = "DM_OBJ_MGR_E_CURSOR_FAIL";

	/**
	 * Failed to update object error
	 */
	public static final String OBJECT_UPDATE_FAILED_ERROR = "DM_API_E_CANT_UPDATE";

	/**
	 * Session deadlock error
	 */
	public static final String SESSION_DEADLOCK_ERROR = "DM_SESSION_E_DEADLOCK";

	/**
	 * Folder path already exists
	 */
	public static final String FOLDER_PATH_EXISTS = "DM_FOLDER_E_PATH_EXISTS";

	/**
	 * Folder already linked
	 */
	public static final String FOLDER_ALREADY_LINKED = "DM_FOLDER_E_ALREADY_LINKED";

	/**
	 * User name already exist
	 */
	public static final String USER_NAME_ALREADY_EXIST = "DM_USER_E_EXISTING_USER_NAME";

	/**
	 * Group name already exist
	 */
	public static final String GROUP_NAME_ALREADY_EXIST = "DM_GROUP_E_UNABLE_TO_SAVE_EXISTING";

	/**
	 * Group recursively contains in another group
	 */
	public static final String GROUP_RECURSION_CONTAINMENT = "DM_GROUP_E_NO_CYCLES_IN_GROUPS";

	/**
	 * No delete access for object
	 */
	public static final String NO_DELETE_ACCESS = "DM_SYSOBJECT_E_NO_DELETE_ACCESS";

	/**
	 * Can't delete locked object
	 */
	public static final String CANT_DELETE_LOCKED = "DM_SYSOBJECT_E_CANT_DELETE_LOCKED";

	/**
	 * Object doesn't exist in docbase or current user doesn't have rights to see it
	 */
	public static final String OBJECT_NOT_EXIST = "DM_API_E_EXIST";

	/**
	 * Can't save object
	 */
	public static final String CANT_SAVE_OBJECT = "DM_SYSOBJECT_E_CANT_SAVE";

	/**
	 * No browse access for object
	 */
	public static final String NO_BROWSE_ACCESS = "DM_SYSOBJECT_E_NO_BROWSE_ACCESS";

	/**
	 * No version access for object
	 */
	public static final String NO_VERSION_ACCESS = "DM_SYSOBJECT_E_NO_VERSION_ACCESS";

	/**
	 * Can't lock object
	 */
	public static final String CANT_LOCK_OBJECT = "DM_SYSOBJECT_E_CANT_LOCK";

	/**
	 * Can't lock object
	 */
	public static final String CANT_UNLOCK_OBJECT = "DM_SYSOBJECT_E_CANT_UNLOCK";

	/**
	 * Unknown response from server
	 */
	public static final String UNKNOWN_SERVER_RESPONSE = "DFC_SESSION_UNKNOWN_SERVER_RESPONSE";

	/**
	 * Can't destroy object
	 */
	public static final String CANT_DESTROY_OBJECT = "DM_SYSOBJECT_E_CANT_DESTROY";

	/**
	 * Object locked
	 */
	public static final String OBJECT_LOCKED = "DM_SYSOBJECT_E_LOCKED";

	/**
	 * Can't move symbolic version label
	 */
	public static final String CANT_MOVE_SYMBOLIC_VERSION_LABEL = "DM_SYSOBJECT_E_CANT_MOVE_SYMBOLIC";

	/**
	 * Object doesn't have such attribute
	 */
	public static final String INCORRECT_OBJECT_ATTRIBUTE = "DM_API_E_BADATTRNAME";

	/**
	 * Failed to save object content
	 */
	public static final String FAILED_TO_SAVE_OBJECT_CONTENT = "DM_CONTENT_E_CONTENT_SAVE_FAILURE";

	/**
	 * There are no access control entries for specified user
	 */
	public static final String NO_ACCESS_CONTROL_ENTRIES_FOR_USER = "DM_ACL_E_NOMATCH";

	/**
	 * User trying to modified ACL is not it's owner or superuser
	 */
	public static final String NOT_ACL_OWNER = "DM_ACL_E_NOT_OWNER";

	/**
	 * ACL with such name already exist
	 */
	public static final String ACL_WITH_SUCH_NAME_ALREADY_EXIST = "DM_ACL_E_UNIQUE_NAME";


	public static String stackTraceToString(Throwable exception)
	{
		StringBuffer buffer = new StringBuffer();

		if (exception == null)
		{
			return buffer.toString();
		}

		buffer.append("\r\n");

		Throwable throwable = exception;
		do
		{
			buffer.append("\t").append(throwable.toString()).append("\r\n");
			StackTraceElement[] elements = throwable.getStackTrace();

			for (int i = 0; i < elements.length; i++)
			{
				buffer.append("\t\t").append(elements[i].toString()).append("\r\n");
			}

			throwable = throwable.getCause();
		}
		while (throwable != null);

		return buffer.toString();
	}

	/**
	 * Verifies if there is DfException in the exceptions chain
	 * with specified message id
	 * @param e root exception
	 * @param messageId message id to look for
	 * @return true if DfException with specified message id exists in chain
	 */
	public static boolean hasMessageIdInChain(DfException e, String messageId)
	{
		if (e == null || ConversionHelper.isEmptyString(messageId))
		{
			return false;
		}

		IDfException ex = e;
		do
		{
			if (messageId.equals(ex.getMessageId()))
			{
				return true;
			}

			Throwable cause = ((DfException)ex).getCause();
			if (cause != null &&
				cause instanceof DfException &&
				messageId.equals(((DfException)cause).getMessageId()))
			{
				return true;
			}

			ex = ex.getNextException();

		} while (ex != null);

		return false;
	}

	/**
	 * Verifies if there is DfException in the exceptions chain
	 * with any of the specified message ids
	 * @param e root exception
	 * @param messageIds message ids
	 * @return true if DfException with any of the specified message ids exists in chain
	 */
	public static boolean hasAnyMessageIdsInChain(DfException e, String[] messageIds)
	{
		if (e == null || messageIds == null || messageIds.length == 0)
		{
			return false;
		}

		IDfException ex = e;
		do
		{
			if (ArrayHelper.hasElement(messageIds, ex.getMessageId()))
			{
				return true;
			}

			Throwable cause = ((DfException)ex).getCause();
			if (cause != null &&
				cause instanceof DfException &&
				ArrayHelper.hasElement(messageIds, ((DfException)cause).getMessageId()))
			{
				return true;
			}

			ex = ex.getNextException();
		} while (ex != null);

		return false;
	}

	/**
	 * Verifies if there is DfException's in the exceptions chain
	 * with all of the specified message ids
	 * @param e root exception
	 * @param messageIds message ids
	 * @return true if DfException's with all of the specified message ids exists in chain
	 */
	public static boolean hasAllMessageIdsInChain(DfException e, String[] messageIds)
	{
		if (e == null || messageIds == null || messageIds.length == 0)
		{
			return false;
		}

		for (String messageId : messageIds)
		{
			if (!hasMessageIdInChain(e, messageId))
			{
				return false;
			}
		}

		return true;
	}

	/**
	 * Verifies if any errors occured which causes
	 * object "fetch" operation to fail
	 * @param e root exception
	 * @return true if there are any exception causes object "fetch" operation to fail
	 */
	public static boolean isObjectFetchErrorOccured(DfException e)
	{
		return hasAnyMessageIdsInChain(e, new String[] {OBJECT_FETCH_FAILED_ERROR, CURSOR_FAIL_ERROR, SESSION_DEADLOCK_ERROR});
	}

	/**
	 * Verifies if object version mismatch error occured while
	 * trying to save object to docbase
	 * @param e root exception
	 * @return true if object version is mismatch
	 */
	public static boolean isObjectVersionMismatchErrorOccured(DfException e)
	{
		return hasAnyMessageIdsInChain(e, OBJECT_VERSION_MISTMATCH);
	}

	/**
	 * Verifies if object update error occured while
	 * trying to save object to docbase
	 * @param e root exception
	 * @return true if update error occured
	 */
	public static boolean isObjectUpdateErrorOccured(DfException e)
	{
		return hasMessageIdInChain(e, OBJECT_UPDATE_FAILED_ERROR);
	}

	/**
	 * Verifies if folder path exists error occured while
	 * trying to save object to docbase
	 * @param e root exception
	 * @return true if update error occured
	 */
	public static boolean isFolderPathExistsErrorOccured(DfException e)
	{
		return hasMessageIdInChain(e, FOLDER_PATH_EXISTS);
	}
}
