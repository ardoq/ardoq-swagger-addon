
{{#hasFields}}
|Field Name|Field Value|
{{#fields}}
{{>table-row}}
{{/}}
{{/}}

{{{XMLMD}}}

{{{securityMd}}}

{{#hasDiscriminator}}
#### Discriminator `{{{discriminator.propertyName}}}`

{{{discriminatorMappingMd}}}
{{/}}

{{#hasExternalDocs}}
#### External Documentation

{{{externalDocs.documentation}}}

[{{{externalDocs.url}}}]({{{externalDocs.url}}})
{{/}}