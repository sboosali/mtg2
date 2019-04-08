##################################################
{}:

##################################################
let
#------------------------------------------------#

rssFile

#------------------------------------------------#

rssString

#------------------------------------------------#

rssHeader = { channels ? [] }: ''
<?xml version="1.0" encoding="utf-8"?>

<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">

  ${channels}

</rss>
'';

#------------------------------------------------#

rssChannel = { title, description, link, items ? [] }: ''
<channel>

  <title>${title}</title>
  <description>${description}</description>
  <link>${link}</link>

  ${items}

</channel>
'';

#------------------------------------------------#

rssItem = { title, link }: ''
<item>
   <title>${title}</title>
   <link>${link}</link>
</item>
  '';

#------------------------------------------------#

rssLink = { title, href ? "rss.xml" }: ''
<link rel="alternate" type="application/rss+xml" title="${title}" href="${href}" />
'';

#------------------------------------------------#
in
##################################################
{

 inherit rssFile rssString rssHeader rssChannel rssItem;
 inherit rssLink;

}
##################################################