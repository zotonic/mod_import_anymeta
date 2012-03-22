{% if m.acl.use.mod_import_anymeta %}
	<li><a href="{% url admin_import_anymeta %}" {% ifequal selected "admin_import_anymeta" %}class="current"{% endifequal %}>{_ Anymeta Import _}</a></li>
{% endif %}
