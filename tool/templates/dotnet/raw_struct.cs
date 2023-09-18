{% include "header.cs" %}

namespace {{ config.namespace }}.Raw;

{{ docstring }}
[StructLayout(LayoutKind.Sequential)]
public partial struct {{ type_name }}
{
    private const string NativeLib = "{{ config.native_lib }}";

	{%- for field in fields %}
		// TODO: gen_field
	{% endfor %}

	{%- for method in methods %}
		// TODO: gen_method
	{% endfor %}
}
