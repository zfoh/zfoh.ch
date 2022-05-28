# How to

Transforming the registration export into `projects.json` is easy, all you have to do is

```
jq '[.[] | select(.additionalInfo.project.name != null) | {id: .additionalInfo.project.name, name: .additionalInfo.project.name, link: .additionalInfo.project.website, "contributor level": .additionalInfo.project.contributorLevel, contact: .info.name, description: .additionalInfo.project.shortDescription}]' 2021-06-19-export.json > projects.json
```
