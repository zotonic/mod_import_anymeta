{% with m.rsc[id] as r %}
    {% with m.rsc[id].is_editable as is_editable %}
        <fieldset>
            <div class="form-group row">
	            <label class="control-label col-md-3" for="field-title-{{ lang_code }}">{_ Title _} {{ lang_code_with_brackets }}</label>
                <div class="col-md-9">
	                <input type="text" id="field-title{{ lang_code }}" name="title{{ lang_code_with_dollar }}" 
		                value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
		                {% if not is_editable %}disabled="disabled"{% endif %}
		                {% include "_language_attrs.tpl" language=lang_code class="do_autofocus field-title form-control" %}
                    />
            </div>
        </div>

        {% if not in_dialog %}
            <div class="form-group row">
	            <label class="control-label col-md-3" for="field-chapeau{{ lang_code_for_id }}">{_ Chapeau _} {{ lang_code_with_brackets }}</label>
                <div class="col-md-9">
	                <input type="text" id="field-chapeau{{ lang_code_for_id }}" name="chapeau{{ lang_code_with_dollar }}" 
		                value="{{ is_i18n|if : r.translation[lang_code].chapeau : r.chapeau }}"
		                {% if not is_editable %}disabled="disabled"{% endif %}
		                {% include "_language_attrs.tpl" language=lang_code class="field-chapeau form-control" %}
                    />
            </div>
        </div>

        <div class="form-group row">
	        <label class="control-label col-md-3" for="field-subtitle{{ lang_code_for_id }}">{_ Subtitle _} {{ lang_code_with_brackets }}</label>
            <div class="col-md-9">
	            <input type="text" id="field-subtitle{{ lang_code_for_id }}" name="subtitle{{ lang_code_with_dollar }}" 
		            value="{{ is_i18n|if : r.translation[lang_code].subtitle : r.subtitle }}"
		            {% if not is_editable %}disabled="disabled"{% endif %}
		            {% include "_language_attrs.tpl" language=lang_code class="field-subtitle form-control" %}
                />
        </div>
    </div>
{% endif %}

<div class="form-group row">
	<label class="control-label col-md-3" for="field-summary{{ lang_code_for_id }}">{_ Summary _} {{ lang_code_with_brackets }}</label>
    <div class="col-md-9">
	    <textarea rows="4" cols="10" id="field-summary{{ lang_code_for_id }}" 
		name="summary{{ lang_code_with_dollar }}"
		{% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="intro form-control" %}
	>{{ is_i18n|if : r.translation[lang_code].summary : r.summary | brlinebreaks }}</textarea>
</div>
    </div>
    
    <div class="form-group row">
	    <label class="control-label col-md-3" for="field-short-title{{ lang_code_for_id }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
        <div class="col-md-9">
	        <input class="form-control" type="text" id="field-short-title{{ lang_code_for_id }}" name="short_title{{ lang_code_with_dollar }}" 
			    value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			    {% if not is_editable %}disabled="disabled"{% endif %}
			    {% include "_language_attrs.tpl" language=lang_code %} />
        </div>
    </div>
</fieldset>

{% endwith %}
{% endwith %}
