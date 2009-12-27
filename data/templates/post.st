<div class="post">
  <h1><a href="/b/$it.id$">$it.title$</a></h1>
  <p>Written by $it.authors:{a|<a href="mailto:$a.mail$">$a.name$</a>}; separator=", "$
     - $it.date$
	 - <a href="/b/$it.id$#disqus_thread">Comments</a>
     - Tags: $it.tags:{t|<a href="/t/$t$">$t$</a>}; separator=", "$</p>
  <div class="content">
    $it.content$
  </div>
</div>
