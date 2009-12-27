  <div id="sidebar">
    <div class="entry">
      <h3>Sites</h3>
      $sites:sidebarsites()$
    </div>
    <div class="entry">
      <h3>Tags</h3>
      <ul>
        $tags:{t|<li><a href="/t/$t.id$">$t.id$</a> \($t.count$\)</li>}$
      </ul>
    </div>
    <div class="entry">
      <h3>Authors</h3>
      $authors:{a|<p>$a.name$: <a href="/a/$a.id$">posts</a>, <a href="mailto:$a.mail$">mail</a></p>}$
    </div>
    <div class="entry">
      <h3>Meta</h3>
      <p><a href="/feed/atom.xml" title="Atom Feed for all Blog Posts">Atom</a></p>
      <p><a href="http://validator.w3.org/check/referer"
          title="This page validates as XHTML 1.0 Strict">Valid XHTML</a></p>
      <p><a href="http://github.com/crazymaik/lambda-blog/"
          title="Powered by lambda-blog">lambda-blog</a></p>
    </div>
  </div>
