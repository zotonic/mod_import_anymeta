{% with m.rsc[id] as r %}
{% with m.rsc[id].is_editable as is_editable %}
<fieldset>
    <div class="control-group">
	<label class="control-label" for="field-title-{{ lang_code }}">{_ Title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-title{{ lang_code }}" name="title{{ lang_code_with_dollar }}" 
		   value="{{ is_i18n|if : r.translation[lang_code].title : r.title }}"
		   {% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="do_autofocus input-block-level field-title" %}
                />
        </div>
    </div>

    {% if not in_dialog %}
    <div class="control-group">
	<label class="control-label" for="field-chapeau{{ lang_code_for_id }}">{_ Chapeau _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-chapeau{{ lang_code_for_id }}" name="chapeau{{ lang_code_with_dollar }}" 
		   value="{{ is_i18n|if : r.translation[lang_code].chapeau : r.chapeau }}"
		   {% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="input-block-level field-chapeau" %}
                />
        </div>
    </div>

    <div class="control-group">
	<label class="control-label" for="field-subtitle{{ lang_code_for_id }}">{_ Subtitle _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-subtitle{{ lang_code_for_id }}" name="subtitle{{ lang_code_with_dollar }}" 
		   value="{{ is_i18n|if : r.translation[lang_code].subtitle : r.subtitle }}"
		   {% if not is_editable %}disabled="disabled"{% endif %}
		{% include "_language_attrs.tpl" language=lang_code class="input-block-level field-subtitle" %}
                />
        </div>
    </div>
    {% endif %}

    <div class="control-group">
	<label class="control-label" for="field-summary{{ lang_code_for_id }}">{_ Summary _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <textarea rows="4" cols="10" id="field-summary{{ lang_code_for_id }}" 
		      name="summary{{ lang_code_with_dollar }}"
		      {% if not is_editable %}disabled="disabled"{% endif %}
		      {% include "_language_attrs.tpl" language=lang_code class="input-block-level intro" %}
		      >{{ is_i18n|if : r.translation[lang_code].summary : r.summary | brlinebreaks }}</textarea>
	</div>
    </div>
    
    <div class="control-group">
	<label class="control-label" for="field-short-title{{ lang_code_for_id }}">{_ Short title _} {{ lang_code_with_brackets }}</label>
        <div class="controls">
	    <input type="text" id="field-short-title{{ lang_code_for_id }}" name="short_title{{ lang_code_with_dollar }}" 
			value="{{ is_i18n|if : r.translation[lang_code].short_title : r.short_title }}"
			{% if not is_editable %}disabled="disabled"{% endif %}
			{% include "_language_attrs.tpl" language=lang_code class="input-block-level" %} />
       </div>
   </div>
</fieldset>

{% endwith %}
{% endwith %}
