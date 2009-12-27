      <ul>
        $if(it.hasContent)$ $it:{s|<li><p><a href="/s/$s.id$">$s.title$</a></p>$s.subs:sidebarsites()$</li>}$ $endif$
		$if(!it.hasContent)$ $it:{s|<li><p>$s.title$</p>$s.subs:sidebarsites()$</li>}$ $endif$
      </ul>

