{% extends "admin_edit_widget_std.tpl" %}

{% block widget_title %}{_ Anymeta Import _}{% endblock %}
{% block widget_show_minimized %}true{% endblock %}

{% block widget_content %}
<div>
    <label>Anymeta id</label>
    {{ id.anymeta_id }}
    <a href="http://{{ id.anymeta_host }}/id/{{ id.anymeta_uuid }}" target="_new">{_ view in new window _}</a>
</div>
{% endblock %}
