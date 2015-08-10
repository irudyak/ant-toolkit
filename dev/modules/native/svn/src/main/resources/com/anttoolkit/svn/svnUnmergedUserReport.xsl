<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format">
	<xsl:template match="/">
		<html>
			<head>
				<title>SVN Unmerged Changes Report</title>
				<style type="text/css">
					.pageBody
					{
						background-color : #FFFFFF;
					}

					.largeFont
					{
						font : 16px Tahoma, Geneva, Arial, Helvetica, sans-serif;
					}

					.defaultFont
					{
						font : 12px Tahoma, Geneva, Arial, Helvetica, sans-serif;
					}

					.smallFont
					{
						font : 9px Tahoma, Geneva, Arial, Helvetica, sans-serif;
					}

					.middleFont
					{
						font : 11px Tahoma, Geneva, Arial, Helvetica, sans-serif;
					}

					.separatorFont
					{
						font : 4px;
					}

					.authorBackground
					{
						background : #D6E3FF;
					}

					.commentBackground
					{
						background : #CEE7BD
					}

					.pathColor
					{
						color:#848284;
					}

					.deletedPathColor
					{
						color:#EE0000;
					}

					.addedPathColor
					{
						color:#00EE00;
					}

					.modifiedPathColor
					{
						color:#0000EE;
					}

					.graphColor
					{
						background:#0000EE;
					}

					.revisionStyle
					{
						color: #FF0080;
						font : 11px Tahoma, Geneva, Arial, Helvetica, sans-serif;
						font-weight: bold;
					}
				</style>
			</head>
			<body class="pageBody">
				<table cellpadding="0" cellspacing="0" width="100%">
					<tr>
						<td class="separatorFont">&#160;</td>
					</tr>
					<tr>
						<td>
							<table class="defaultFont" cellpadding="0" cellspacing="0" width="100%" align="left" border="0">
								<xsl:for-each select="/report/author">
									<tr class="authorBackground">
										<td colspan="4"><b><xsl:value-of select="@name"/></b><xsl:text> : </xsl:text><span class="smallFont"><xsl:value-of select="@changes"/> unmerged changes</span></td>
									</tr>
									<xsl:for-each select="logentry">
										<tr>
											<td>&#160;&#160;&#160;</td>
											<td width="100%" colspan="3" class="commentBackground">
												<table width="100%" style="table-layout: fixed; word-wrap: break-word;">
													<tr>
														<td width="100%">
															<span class="revisionStyle">revision: <xsl:value-of select="@revision"/>&#160;[<xsl:value-of select="@date"/>]&#160;</span>&#160;<b><xsl:value-of select="@text"/></b>
														</td>
													</tr>
												</table>
											</td>
										</tr>
										<xsl:for-each select="path">
											<tr>
												<td>&#160;&#160;&#160;</td>
												<td>&#160;&#160;&#160;</td>
												<xsl:choose>
													<xsl:when test="attribute::action='A'">
														<b><td valign="middle" class="middleFont addedPathColor">+&#160;&#160;</td></b>
													</xsl:when>
													<xsl:when test="attribute::action='D'">
														<b><td valign="middle" class="middleFont deletedPathColor">-&#160;&#160;</td></b>
													</xsl:when>
													<xsl:otherwise>
														<b><td valign="middle" class="middleFont modifiedPathColor">*&#160;&#160;</td></b>
													</xsl:otherwise>
												</xsl:choose>
												<td width="100%" class="pathColor"><xsl:value-of select="current()"/></td>
											</tr>
										</xsl:for-each>
									</xsl:for-each>
								</xsl:for-each>
							</table>
						</td>
					</tr>
				</table>
			</body>
		</html>
	</xsl:template>
</xsl:stylesheet>
