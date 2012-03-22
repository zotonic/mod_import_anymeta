{% extends "admin_base.tpl" %}

{% block title %}{_ Import Anymeta Data _}{% endblock %}

{% block content %}

<div id="content" class="zp-85">
	<div class="block clearfix">

	<h2>{_ Find an imported Anymeta id _}</h2>

	{% wire id="find-anymeta-id" type="submit" postback=`find_imported` delegate=`mod_import_anymeta` %}
    <form id="find-anymeta-id" method="post" action="postback" class="admin-form">
		<div class="form-item clearfix">
            <label>Anymeta ID</label>
            <input type="text" id="imported_id" name="imported_id" value="" style="width:50px" />
            {% validate id="imported_id" type={presence} type={numericality minimum=1} %}
        </div>
		<p style="display:none" class="notification error" id="find-error">
		    {_ This id has not been imported. _}
		</p>
		<div class="form-item clearfix">
			<button type="submit">{_ Find _}</button>
		</div>
    </form>

	<h2>{_ Import data from an Anymeta web site _}</h2>

	<p style="display:none" id="import-started" class="notification notice">{_ The import has started, come back here for more information about the progress of your import. _}</p>

	{% wire id="admin-anymeta-import" type="submit" postback=`import_anymeta` delegate=`mod_import_anymeta` %}
	<form id="admin-anymeta-import" method="POST" action="postback" class="admin-form">
		<div class="form-item clearfix">
			<label for="field-date-start">{_ Hostname of Anymeta site (without “http://”) _}</label>
			<input type="text" id="host" name="host" value="{{ m.session.anymeta_host|force_escape|default:"www.example.com"}}" />
			<p style="display:none" class="notification error" id="import-nxdomain">{_ Could not start import, the hostname could not be found. _}</p>
		</div>

		<div class="form-item clearfix">
			<label for="field-date-start">{_ From Anymeta ID _}</label>
			<input type="text" name="start-id" id="start-id" value="1" style="width:50px" />
			{% validate id="start-id" type={numericality minimum=1} %}
		</div>

		<div class="form-item clearfix">
			<label for="field-date-stop">{_ Till (and including) Anymeta ID _}</label>
			<input type="text" name="end-id" id="end-id" value="" style="width:50px" />
			{% validate id="end-id" type={numericality minimum=1} %}
		</div>

		<div class="form-item clearfix">
			<label for="field-date-start">{_ Optional sysadmin password _}</label>
			<input type="text" name="sysadmin-pw" value="" style="width:120px" />
		</div>
		  
		<p style="display:none" class="notification error" id="import-error">{_ Could not start import, unexpected result from remote server. _}</p>
		
		<div class="form-item clearfix">
			<button type="submit">{_ Start Import _}</button>
		</div>
	</form>
	</div>
	
	<div class="block">
	    <h3>{_ Messages from the import _}</h3>
    	<pre id="progress">
    	</pre>
	</div>
</div>

{% wire action={connect signal={import_anymeta_progress}} %}

{% wire action={focus target="host"} %}

{% endblock %}
