{% extends "admin_base.tpl" %}

{% block title %}{_ Import Anymeta Data _}{% endblock %}

{% block content %}

    <div class="admin-header">
        <h2>{_ Import data from an Anymeta site _}</h2>

        <p>{_ Here you can import data from an Anymeta site into this Zotonic site. _}</p>
    </div>

	<h2>{_  _}</h2>

    {% wire id="find-anymeta-id" type="submit" postback=`find_imported` delegate=`mod_import_anymeta` %}
    <form id="find-anymeta-id" method="post" action="postback" class="admin-form">
        <div class="widget">
            <h3 class="widget-header">{_ Find migrated Anymeta ID _}</h3>
            <div class="widget-content">

                <div class="form-group row">
                    <label class="control-label col-md-3" for="imported_id">{_ External Anymeta ID _}</label>
                    <div class="col-md-3">
                        <input type="text" id="imported_id" name="imported_id" value="" class="form-control" />
                        {% validate id="imported_id" type={presence} type={numericality minimum=1} %}
                    </div>
                </div>
                <div class="form-group row">
                    <label class="control-label col-md-3" for="imported_host">{_ Hostname of Anymeta site (without “http://”) _}</label>
                    <div class="col-md-3">
                        <input type="text" id="imported_uri" name="imported_host" value="" class="form-control" />
                    </div>
                    <div class="col-md-6">
                        <button class="btn btn-primary" type="submit">{_ Find _}</button>
                    </div>
                </div>

                <div class="form-group row">
                    <div class="col-md-12">
    		            <div style="display:none" class="alert alert-danger" id="find-error">
    		                {_ This id has not been imported. _}
    		            </div>
                    </div>
                </div>

            </div>
        </div>
    </form>

    <div style="display:none" id="import-started" class="alert alert-success">
        {_ The import has started, come back here for more information about the progress of your import. _}
    </div>

    {% wire id="admin-anymeta-import" type="submit" postback=`import_anymeta` delegate=`mod_import_anymeta` %}
    <form id="admin-anymeta-import" method="POST" action="postback" class="admin-form">
        <div class="widget">
            <h3 class="widget-header">{_ Import data _}</h3>
            <div class="widget-content">

                <div class="form-group row">
                    <label class="control-label col-md-3" for="host">{_ Hostname of Anymeta site (without “http://”) _}</label>
                    <div class="col-md-9">
                        <input type="text" id="host" name="host" value="{{ m.session.anymeta_host|force_escape|default:"www.example.com"}}" class="col-lg-8 col-md-8 form-control" />
                    </div>
    			    <p style="display:none" class="notification error" id="import-nxdomain">{_ Could not start import, the hostname could not be found. _}</p>
                </div>


                <div class="form-group row">
                    <label class="control-label col-md-3" for="host">{_ Orginal Anymeta Hostname (without “http://”) _}</label>
                    <div class="col-md-9">
                        <input type="text" id="host" name="host_original" value="{{ m.session.anymeta_host_original|force_escape }}" class="col-lg-8 col-md-8 form-control" />
                    </div>
                    <p style="display:none" class="notification error" id="import-nxdomain">{_ Set this if you are re-importing from a backup of the old site. _}</p>
                </div>

                <div class="form-group row">
                    <label class="control-label col-md-3" for="start-id">{_ From Anymeta ID _}</label>
                    <div class="col-md-9">
                        <input type="text" id="start-id" name="start-id" value="1" class="col-lg-2 col-md-2 form-control" />
                    </div>
                    {% validate id="start-id" type={numericality minimum=1} %}
                </div>

                <div class="form-group row">
                    <label class="control-label col-md-3" for="end-id">{_ To Anymeta ID _}</label>
                    <div class="col-md-9">
                        <input type="text" id="end-id" name="end-id" value="" class="col-lg-2 col-md-2 form-control" />
                    </div>
                    {% validate id="end-id" type={numericality minimum=1} %}
                </div>

                <div class="form-group row">
                    <label class="control-label col-md-3" for="sysadmin-pw">{_ Import secret _}</label>
                    <div class="col-md-9">
                        <input type="text" id="sysadmin-pw" name="sysadmin-pw" value="" class="col-lg-2 col-md-2 form-control" />
                        <p class="help-block">{_ Authorization secret for importing non-public things _}</p>
                    </div>
                </div>

                <div class="form-group row">
                    <label class="control-label col-md-3" for="sysadmin-pw">{_ Import _}</label>
                    <div class="col-md-4">
                        <select class="form-control" name="blobs" id="import-blobs">
                            <option value="y">{_ Things with their blobs _}</option>
                            <option value="n">{_ Things only, don’t download blobs _}</option>
                            <option value="b">{_ Blobs only, don’t update imported things _}</option>
                            <option value="e">{_ Edges only, don’t update imported things and blobs _}</option>
                            <option value="t">{_ Tags and keywords only, don’t update imported things and blobs _}</option>
                        </select>
                    </div>
                </div>

    		    <div style="display:none" class="alert alert-danger" id="import-error">
    		        {_ Could not start import, unexpected result from remote server. _}
    		    </div>
    		    
                <div class="form-group row">
    		        <div class="col-md-9 col-md-offset-3">
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
