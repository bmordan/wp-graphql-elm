# Elm GraphQl Wordpress

Get started with

```sh
npm run build
```

This is an Elm site that consumes a Wordpress API via a GraphQl. The site is built out into the `/dist` folder from the elm files and I'm deploying the built artifacts to [surge.sh](https://surge.sh)

## GraphQl

![](https://user-images.githubusercontent.com/4499581/35184744-6d17abb0-fdf1-11e7-9653-1acaccba1f91.png)

Beautiful and easy. To do this with REST took 3 separate requests. I'm using

```json
{ "ghivert/elm-graphql": "3.1.0 <= v < 4.0.0" }
```

for GraphQl support.

## Pages

The page queries in elm look something like this:

```elm
pageRequest : Operation Query Variables
pageRequest =
    GraphQl.named "query"
        [ GraphQl.field "pageBy"
            |> GraphQl.withArgument "uri" (GraphQl.string "contact-us")
            |> GraphQl.withSelectors
                [ GraphQl.field "title"
                , GraphQl.field "content"
                , GraphQl.field "author"
                    |> GraphQl.withSelectors
                        [ GraphQl.field "name"
                        , GraphQl.field "avatar"
                            |> GraphQl.withSelectors
                                [ GraphQl.field "url"
                                ]
                        ]
                ]
        ]
        |> GraphQl.withVariables []
```

Which I can stomach. The page 'slug' can be passed in and I'll get back all the state for that page in one hit. The model is flat.

```elm
type alias Model =
    { title : String
    , content : String
    , author : String
    , avatar : String
    }
```

Just render the model into Html.

## Posts

Posts are more tricky. A wordpress post has the following url structure.

```
https://www.domain.com/2011/01/rsync-exclude-files-and-folders
```

This is not going to work for a static page as it would look into a folder called `2011` and within that another folder called `01`...

...Instead I'm going to

```
https://www.domain.com/post.html#rsync-exclude-files-and-folders
```

snap off the 'hash' *`#rsync-exclude-files-and-folders`* and use the slug/hash to query the graphql endpoint for the post's data.
