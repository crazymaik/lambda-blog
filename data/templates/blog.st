$htmlheader()$
<body>
  <div id="page">
    $header()$
    $sidebar()$
    <div id="content">
      $posts:post()$
      <div id="index">
        $idxbefore:{n|<a href="$baseUrl$/$n$">$n$</a>}; separator=" "$
        $idxcurrent$
        $idxafter:{n|<a href="$baseUrl$/$n$">$n$</a>}; separator=" "$
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
