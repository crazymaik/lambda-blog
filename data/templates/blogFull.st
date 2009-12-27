$htmlheader()$
<body>
  <div id="page">
    $header()$
    $sidebar()$
    <div id="content">
      $posts:post()$
	  <div class="comments">
	  	<div id="disqus_thread"></div>
	  	<script type="text/javascript" src="http://disqus.com/forums/bitbrothers/embed.js"></script>
	  	<noscript>
	  		<p><a href="http://disqus.com/forums/bitbrothers/?url=ref">View the discussion thread.</a></p>
	  	</noscript>
	  	<a href="http://disqus.com" class="dsq-brlink">blog comments powered by <span class="logo-disqus">Disqus</span></a>
	  </div>
    </div>
    $footer()$
  </div>

<script type="text/javascript">
//<![CDATA[
(function() {
	var links = document.getElementsByTagName('a');
	var query = '?';
	for(var i = 0; i < links.length; i++) {
	if(links[i].href.indexOf('#disqus_thread') >= 0) {
		query += 'url' + i + '=' + encodeURIComponent(links[i].href) + '&';
	}
	}
	document.write('<script charset="utf-8" type="text/javascript" src="http://disqus.com/forums/bitbrothers/get_num_replies.js' + query + '"></' + 'script>');
})();
//]]>
</script>

</body>
</html>
