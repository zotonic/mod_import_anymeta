{% extends "admin_base.tpl" %}

{% block title %}{_ Import Anymeta Data _}{% endblock %}

{% block content %}

<div class="admin-header">
    <h2>{_ Import data from an Anymeta site _}</h2>

    <p>{_ Here you can import data from an Anymeta site into this Zotonic site. _}</p>
</div>

	<h2>{_  _}</h2>

{% wire id="find-anymeta-id" type="submit" postback=`find_imported` delegate=`mod_import_anymeta` %}
<form id="find-anymeta-id" method="post" action="postback" class="admin-form form-horizontal form-inline">
    <div class="widget">
        <h3 class="widget-header">{_ Find migrated Anymeta ID _}</h3>
        <div class="widget-content">

            <div class="control-group">
                <label class="control-label" for="imported_id">{_ External Anymeta ID _}</label>
                <div class="controls">
                    <input type="text" id="imported_id" name="imported_id" value="" class="span2" />
                    {% validate id="imported_id" type={presence} type={numericality minimum=1} %}
                    <button class="btn btn-primary" type="submit">{_ Find _}</button>
                    <p class="help-block">{_ You can only find Anymeta IDs that were migrated. Use the UUID or the normal search to find other imports. _}</p>
                </div>
            </div>

    		<div style="display:none" class="alert alert-error" id="find-error">
    		    {_ This id has not been imported. _}
    		</div>

        </div>
    </div>
</form>

<div style="display:none" id="import-started" class="alert alert-success">
    {_ The import has started, come back here for more information about the progress of your import. _}
</div>

{% wire id="admin-anymeta-import" type="submit" postback=`import_anymeta` delegate=`mod_import_anymeta` %}
<form id="admin-anymeta-import" method="POST" action="postback" class="admin-form form-horizontal">
    <div class="widget">
        <h3 class="widget-header">{_ Import data _}</h3>
        <div class="widget-content">

            <div class="control-group">
                <label class="control-label" for="host">{_ Hostname of Anymeta site (without “http://”) _}</label>
                <div class="controls">
                    <input type="text" id="host" name="host" value="{{ m.session.anymeta_host|force_escape|default:"www.example.com"}}" class="span8" />
                </div>
    			<p style="display:none" class="notification error" id="import-nxdomain">{_ Could not start import, the hostname could not be found. _}</p>
            </div>

            <div class="control-group">
                <label class="control-label" for="start-id">{_ From Anymeta ID _}</label>
                <div class="controls">
                    <input type="text" id="start-id" name="start-id" value="1" class="span2" />
                </div>
                {% validate id="start-id" type={numericality minimum=1} %}
            </div>

            <div class="control-group">
                <label class="control-label" for="end-id">{_ To Anymeta ID _}</label>
                <div class="controls">
                    <input type="text" id="end-id" name="end-id" value="" class="span2" />
                </div>
                {% validate id="end-id" type={numericality minimum=1} %}
            </div>

            <div class="control-group">
                <label class="control-label" for="keep-id">{_ Content migration _}</label>
                <div class="controls">
                    <label class="checkbox">
                        <input type="checkbox" id="keep-id" name="keep-id" value="1" />
                        {_ Remember Anymeta ID for supporting old URLs and lookup by Anymeta ID. _}
                    </label>
                    <p class="help-block">{_ Check this <strong>only for the main site</strong> you are migrating from, otherwise you overwrite other imported ids. _}</p>
                </div>
            </div>

            <div class="control-group">
                <label class="control-label" for="sysadmin-pw">{_ Import secret _}</label>
                <div class="controls">
                    <input type="text" id="sysadmin-pw" name="sysadmin-pw" value="" class="span2" />
                    <p class="help-block">{_ Authorization secret for importing non-public things _}</p>
                </div>
            </div>
		  
		    <div style="display:none" class="alert alert-error" id="import-error">
		        {_ Could not start import, unexpected result from remote server. _}
		    </div>
		
            <div class="control-group">
		        <div class="controls">
                    <button class="btn btn-primary" type="submit">{_ Start Import _}</button>
                </div>
		    </div>
		</div>
	</div>
</form>
	
<div class="widget">
    <h3 class="widget-header">{_ Messages from the import _}</h3>
    <div class="widget-content">
        <p>{_ Progress messages from running imports are shown here. _}</p>
        <pre id="progress"></pre>
    </div>
</div>

{% wire action={connect signal={import_anymeta_progress}} %}

{% wire action={focus target="host"} %}

{% endblock %}
