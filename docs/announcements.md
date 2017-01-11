---
layout: page
title: Announcements
permalink: /announcements/
---


{% for post in site.posts %}
<a name="{{ post.title}}"></a>

## {{ post.date | date: '%B %d, %Y' }} - <a href="{{ post.url }}">{{ post.title }}</a>

{{ post.content }}

<hr />
{% endfor %}


