{% include "header.cs" %}

namespace {{ config.namespace }}.Raw;

{{ fmt.docstring(def.docs) }}
[StructLayout(LayoutKind.Sequential)]
public enum {{ fmt.enum_name(def) }}
{
	{%- for variant in def.variants %}
		{{ fmt.docstring(variant.docs) }}
		{{ fmt.enum_variant_name(variant) }} = {{ variant.discriminant }},
	{% endfor %}
}
